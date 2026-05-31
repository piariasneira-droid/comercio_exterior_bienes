from pathlib import Path
import duckdb

def csvs_a_data_lake(
    year: int,
    month: int,
    version: str,
    raw_base_dir: str = "data/raw",
    out_dir: str = "data/rawparquet",
    filtros_provincia: dict[str, list[int]] = None
):
    """
    Convierte CSVs mensuales (taric y sectores) a estructura de data lake particionada.
    Genera tres versiones: completa, por provincia filtrada, y España (agregado nacional).
    
    Args:
        year: Año de los datos
        month: Mes de los datos (1-12)
        version: 'def' (definitivo) o 'prov' (provisional)
        raw_base_dir: Directorio base de CSVs de entrada
        out_dir: Directorio base del data lake de salida
        filtros_provincia: Dict con nombre y lista de provincias a filtrar.
                          Ej: {"madrid": [28], "cataluna": [8, 17, 25, 43]}
    """
    if version not in {"def", "prov"}:
        raise ValueError("version debe ser 'def' o 'prov'")
    if not 1 <= month <= 12:
        raise ValueError("month debe estar entre 1 y 12")
    
    # Mapear version a estado numérico (def=1, prov=0)
    estado_num = 1 if version == "def" else 0
    month_str = f"{month:02d}"
    
    # Si no se especifican filtros, usar Madrid por defecto
    if filtros_provincia is None:
        filtros_provincia = {"madrid": [28]}
    
    con = duckdb.connect()
    
    # Procesar ambos niveles: taric y sectores
    for nivel in ["taric", "sectores"]:
        in_dir = Path(raw_base_dir) / nivel / version
        
        # Determinar el prefijo correcto para los archivos CSV
        csv_prefix = "taric" if nivel == "taric" else "sec"
        csv_file = in_dir / f"comex_{csv_prefix}_{year}{month_str}.csv"
        
        if not csv_file.exists():
            print(f"[WARNING] No existe el archivo {csv_file}, saltando...")
            continue
        
        # 1. DATASET COMPLETO (todas las provincias)
        _procesar_completo(con, csv_file, nivel, out_dir, estado_num, year, month_str)
        
        # 2. DATASETS FILTRADOS POR PROVINCIA
        for nombre_filtro, provincias in filtros_provincia.items():
            _procesar_provincia(con, csv_file, nivel, out_dir, estado_num, year, month_str, 
                              nombre_filtro, provincias)
        
        # 3. DATASET ESPAÑA (agregado nacional)
        _procesar_espana(con, csv_file, nivel, out_dir, estado_num, year, month_str)
        
        # 4. DATASET TOTALES CCAA (solo para sectores)
        if nivel == "sectores":
            _procesar_totales_ccaa(con, csv_file, out_dir, estado_num, year, month_str)
        
        print(f"[OK] Procesado {csv_file.name} en todas sus versiones")
    
    con.close()

def _procesar_completo(con, csv_file, nivel, out_dir, estado_num, year, month_str):
    """Procesa el dataset completo con todas las provincias"""
    if nivel == "taric":
        query = f"""
        COPY (
            SELECT
                CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                CAST(pais AS INTEGER) AS pais,
                CAST(provincia AS INTEGER) AS provincia,
                CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                CAST(REPLACE(kilogramos, ',', '.') AS DOUBLE) AS kilogramos,
                CAST(nivel_taric AS INTEGER) AS nivel_taric,
                CAST(cod_taric AS DOUBLE) AS cod_taric,
                {estado_num} AS estado,
                CAST(año AS INTEGER) AS anio,
                CAST(mes AS INTEGER) AS mes
            FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16')
        )
        TO '{out_dir}/{nivel}'
        (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
        """
    else:  # sectores
        query = f"""
        COPY (
            SELECT
                CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                CAST(pais AS INTEGER) AS pais,
                CAST(provincia AS INTEGER) AS provincia,
                CAST(nivel_sector_economico AS INTEGER) AS nivel_sector_economico,
                cod_sector_economico,
                CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                {estado_num} AS estado,
                CAST(año AS INTEGER) AS anio,
                CAST(mes AS INTEGER) AS mes
            FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16',
                             types={{'cod_sector_economico': 'VARCHAR'}})
        )
        TO '{out_dir}/{nivel}'
        (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
        """
    
    con.execute(query)
    print(f"    → {out_dir}/{nivel}/estado={estado_num}/anio={year}/mes={month_str}/")


def _procesar_provincia(con, csv_file, nivel, out_dir, estado_num, year, month_str, 
                       nombre_filtro, provincias):
    """Procesa el dataset filtrando provincias específicas y elimina la columna provincia"""
    # Crear la condición WHERE para múltiples provincias
    provincias_str = ", ".join(str(p) for p in provincias)
    where_clause = f"CAST(provincia AS INTEGER) IN ({provincias_str})"
    
    if nivel == "taric":
        query = f"""
        COPY (
            SELECT
                CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                CAST(pais AS INTEGER) AS pais,
                CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                CAST(REPLACE(kilogramos, ',', '.') AS DOUBLE) AS kilogramos,
                CAST(nivel_taric AS INTEGER) AS nivel_taric,
                CAST(cod_taric AS DOUBLE) AS cod_taric,
                {estado_num} AS estado,
                CAST(año AS INTEGER) AS anio,
                CAST(mes AS INTEGER) AS mes
            FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16')
            WHERE {where_clause}
        )
        TO '{out_dir}/{nombre_filtro}/{nivel}'
        (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
        """
    else:  # sectores
        query = f"""
        COPY (
            SELECT
                CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                CAST(pais AS INTEGER) AS pais,
                CAST(nivel_sector_economico AS INTEGER) AS nivel_sector_economico,
                cod_sector_economico,
                CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                {estado_num} AS estado,
                CAST(año AS INTEGER) AS anio,
                CAST(mes AS INTEGER) AS mes
            FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16',
                             types={{'cod_sector_economico': 'VARCHAR'}})
            WHERE {where_clause}
        )
        TO '{out_dir}/{nombre_filtro}/{nivel}'
        (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
        """
    
    con.execute(query)
    print(f"    → {out_dir}/{nombre_filtro}/{nivel}/estado={estado_num}/anio={year}/mes={month_str}/ (provincias: {provincias})")


def _procesar_espana(con, csv_file, nivel, out_dir, estado_num, year, month_str):
    """Procesa el dataset agregando por España (suma de todas las provincias)"""
    if nivel == "taric":
        query = f"""
        COPY (
            SELECT
                flujo,
                pais,
                SUM(euros) AS euros,
                SUM(dolares) AS dolares,
                SUM(kilogramos) AS kilogramos,
                nivel_taric,
                cod_taric,
                estado,
                anio,
                mes
            FROM (
                SELECT
                    CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                    CAST(pais AS INTEGER) AS pais,
                    CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                    CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                    CAST(REPLACE(kilogramos, ',', '.') AS DOUBLE) AS kilogramos,
                    CAST(nivel_taric AS INTEGER) AS nivel_taric,
                    CAST(cod_taric AS DOUBLE) AS cod_taric,
                    {estado_num} AS estado,
                    CAST(año AS INTEGER) AS anio,
                    CAST(mes AS INTEGER) AS mes
                FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16')
            )
            GROUP BY flujo, pais, nivel_taric, cod_taric, estado, anio, mes
        )
        TO '{out_dir}/espana/{nivel}'
        (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
        """
    else:  # sectores
        query = f"""
        COPY (
            SELECT
                flujo,
                pais,
                nivel_sector_economico,
                cod_sector_economico,
                SUM(euros) AS euros,
                SUM(dolares) AS dolares,
                estado,
                anio,
                mes
            FROM (
                SELECT
                    CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                    CAST(pais AS INTEGER) AS pais,
                    CAST(nivel_sector_economico AS INTEGER) AS nivel_sector_economico,
                    cod_sector_economico,
                    CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                    CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                    {estado_num} AS estado,
                    CAST(año AS INTEGER) AS anio,
                    CAST(mes AS INTEGER) AS mes
                FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16',
                                 types={{'cod_sector_economico': 'VARCHAR'}})
            )
            GROUP BY flujo, pais, nivel_sector_economico, cod_sector_economico, estado, anio, mes
        )
        TO '{out_dir}/espana/{nivel}'
        (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
        """
    
    con.execute(query)
    print(f"    → {out_dir}/espana/{nivel}/estado={estado_num}/anio={year}/mes={month_str}/")


def _procesar_totales_ccaa(con, csv_file, out_dir, estado_num, year, month_str):
    """
    Procesa totales por comunidad autónoma desde sectores con nivel_sector_economico=1.
    Mapea provincia a comunidad autónoma y agrega euros y dólares (suma de todos los países).
    Incluye totales nacionales con cod_comunidad=99.
    """
    # Mapeo provincia -> comunidad autónoma (índices 0-52)
    mapeo_ccaa = [
        "0", "14", "7", "13", "1", "6", "9", "4", "8", "6", "9", "1", "13", "7", "1", "10", 
        "7", "8", "1", "7", "14", "1", "2", "1", "6", "8", "16", "10", "15", "1", "11", "12", 
        "10", "3", "6", "5", "10", "6", "5", "17", "6", "1", "6", "8", "2", "7", "13", "6", 
        "14", "6", "2", "18", "19"
    ]
    
    # Construir el CASE statement para el mapeo
    case_parts = []
    for cod_prov, cod_ccaa in enumerate(mapeo_ccaa):
        case_parts.append(f"WHEN {cod_prov} THEN {cod_ccaa}")
    
    case_statement = "CASE CAST(provincia AS INTEGER) " + " ".join(case_parts) + " ELSE 0 END"
    
    query = f"""
    COPY (
        -- Unir datos por CCAA con totales nacionales (cod_comunidad=99)
        SELECT * FROM (
            -- Totales por comunidad autónoma
            SELECT
                flujo,
                cod_comunidad,
                SUM(euros) AS euros,
                SUM(dolares) AS dolares,
                estado,
                anio,
                mes
            FROM (
                SELECT
                    CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                    {case_statement} AS cod_comunidad,
                    CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                    CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                    {estado_num} AS estado,
                    CAST(año AS INTEGER) AS anio,
                    CAST(mes AS INTEGER) AS mes
                FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16',
                                 types={{'cod_sector_economico': 'VARCHAR'}})
                WHERE CAST(nivel_sector_economico AS INTEGER) = 1
            )
            GROUP BY flujo, cod_comunidad, estado, anio, mes
            
            UNION ALL
            
            -- Totales nacionales (suma de todas las CCAA)
            SELECT
                flujo,
                99 AS cod_comunidad,
                SUM(euros) AS euros,
                SUM(dolares) AS dolares,
                estado,
                anio,
                mes
            FROM (
                SELECT
                    CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                    CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                    CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                    {estado_num} AS estado,
                    CAST(año AS INTEGER) AS anio,
                    CAST(mes AS INTEGER) AS mes
                FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16',
                                 types={{'cod_sector_economico': 'VARCHAR'}})
                WHERE CAST(nivel_sector_economico AS INTEGER) = 1
            )
            GROUP BY flujo, estado, anio, mes
        )
        ORDER BY flujo, cod_comunidad
    )
    TO '{out_dir}/totalesccaa'
    (FORMAT PARQUET, PARTITION_BY (estado, anio, mes), OVERWRITE_OR_IGNORE true);
    """
    
    con.execute(query)
    print(f"    → {out_dir}/totalesccaa/estado={estado_num}/anio={year}/mes={month_str}/")