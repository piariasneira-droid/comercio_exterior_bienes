from pathlib import Path
from typing import Callable, Sequence, Literal
import logging
from datetime import datetime
import duckdb
import polars as pl

# Función validar meses
def _validate_meses(meses: list[int]) -> list[int]:
    if not meses:
        raise ValueError("La lista de meses no puede estar vacía")
    for m in meses:
        if m < 1 or m > 12:
            raise ValueError(f"Mes inválido: {m}")
    return meses


# Configuración de logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)


class ParquetStore:
    """
    Capa de acceso a datos Parquet con estructura tipo data lake.
    Incluye funcionalidades de ETL desde CSV y procesamiento de dominios derivados.
    """

    def __init__(self, base_dir: str | Path):
        self.base_dir = Path(base_dir)
        self.logger = logging.getLogger(self.__class__.__name__)
        self._duckdb_con = None
        
    @property
    def duckdb_con(self) -> duckdb.DuckDBPyConnection:
        """Conexión lazy a DuckDB."""
        if self._duckdb_con is None:
            self._duckdb_con = duckdb.connect()
        return self._duckdb_con

    def close(self):
        """Cierra la conexión a DuckDB si existe."""
        if self._duckdb_con is not None:
            self._duckdb_con.close()
            self._duckdb_con = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    # ==================== MÉTODOS BÁSICOS ====================

    def build_path(
        self,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
    ) -> Path:
        """Construye la ruta al archivo parquet."""
        return (
            self.base_dir
            / ambito
            / dominio
            / f"estado={estado}"
            / f"anio={anio}"
            / f"mes={mes}"
            / filename
        )

    def exists(
        self,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
    ) -> bool:
        """Verifica si existe un archivo parquet."""
        return self.build_path(ambito, dominio, estado, anio, mes, filename).exists()

    def read(
        self,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
    ) -> pl.DataFrame:
        """Lee un archivo parquet."""
        path = self.build_path(ambito, dominio, estado, anio, mes, filename)
        
        if not path.exists():
            raise FileNotFoundError(f"No existe: {path}")
        
        self.logger.debug(f"Leyendo: {path}")
        return pl.read_parquet(path)

    def scan(
        self,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
    ) -> pl.LazyFrame:
        """Escanea un archivo parquet (lazy)."""
        path = self.build_path(ambito, dominio, estado, anio, mes, filename)
        
        if not path.exists():
            raise FileNotFoundError(f"No existe: {path}")
        
        self.logger.debug(f"Escaneando: {path}")
        return pl.scan_parquet(path)

    def write(
        self,
        df: pl.DataFrame,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
        overwrite: bool = True,
    ) -> Path:
        """Escribe un DataFrame a parquet."""
        path = self.build_path(ambito, dominio, estado, anio, mes, filename)
        path.parent.mkdir(parents=True, exist_ok=True)

        if path.exists() and not overwrite:
            raise FileExistsError(f"Ya existe: {path}")

        self.logger.debug(f"Escribiendo: {path}")
        df.write_parquet(path)
        return path

    # ==================== ETL DESDE CSV ====================

    def csv_to_parquet(
        self,
        year: int,
        month: int,
        version: str,
        raw_base_dir: str | Path = "data/raw",
        filtros_provincia: dict[str, list[int]] | None = None
    ) -> None:
        """
        Convierte CSVs mensuales (taric y sectores) a estructura de data lake.
        
        Args:
            year: Año de los datos
            month: Mes de los datos (1-12)
            version: 'def' (definitivo) o 'prov' (provisional)
            raw_base_dir: Directorio base de CSVs de entrada
            filtros_provincia: Dict con nombre y lista de provincias a filtrar.
        """
        if version not in {"def", "prov"}:
            raise ValueError("version debe ser 'def' o 'prov'")
        if not 1 <= month <= 12:
            raise ValueError("month debe estar entre 1 y 12")
        
        estado_num = 1 if version == "def" else 0
        month_str = f"{month:02d}"
        
        if filtros_provincia is None:
            filtros_provincia = {"madrid": [28]}
        
        raw_base_dir = Path(raw_base_dir)
        
        for nivel in ["taric", "sectores"]:
            in_dir = raw_base_dir / nivel / version
            csv_prefix = "taric" if nivel == "taric" else "sec"
            csv_file = in_dir / f"comex_{csv_prefix}_{year}{month_str}.csv"
            
            if not csv_file.exists():
                self.logger.warning(f"No existe {csv_file}, saltando...")
                continue
            
            self.logger.info(f"Procesando {csv_file.name}")
            
            # 1. Dataset completo
            df_completo = self.read_and_transform_csv(csv_file, nivel, estado_num, year, month)
            self.write(df_completo, "", nivel, estado_num, year, month)
            self.logger.info(f"  ✓ Completo: {nivel}/estado={estado_num}/anio={year}/mes={month_str}/")
            
            # 2. Filtrados por provincia
            for nombre_filtro, provincias in filtros_provincia.items():
                df_filtrado = df_completo.filter(pl.col("provincia").is_in(provincias))
                df_filtrado = df_filtrado.drop("provincia")
                self.write(df_filtrado, nombre_filtro, nivel, estado_num, year, month)
                self.logger.info(f"  ✓ {nombre_filtro.capitalize()}: {nivel} (provincias: {provincias})")
            
            # 3. España (agregado nacional)
            df_espana = self.aggregate_spain(df_completo, nivel)
            self.write(df_espana, "espana", nivel, estado_num, year, month)
            self.logger.info(f"  ✓ España: {nivel} agregado nacional")
            
            # 4. Totales CCAA (solo sectores)
            if nivel == "sectores":
                df_ccaa = self.process_ccaa_totals(df_completo, estado_num, year, month)
                self.write(df_ccaa, "", "totalesccaa", estado_num, year, month)
                self.logger.info(f"  ✓ Totales CCAA")

    def read_and_transform_csv(
        self,
        csv_file: Path,
        nivel: str,
        estado: int,
        year: int,
        month: int
    ) -> pl.DataFrame:
        """Lee CSV y lo transforma a DataFrame de Polars con tipos correctos."""
        
        if nivel == "taric":
            query = f"""
            SELECT
                CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                CAST(pais AS INTEGER) AS pais,
                CAST(provincia AS INTEGER) AS provincia,
                CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                CAST(REPLACE(kilogramos, ',', '.') AS DOUBLE) AS kilogramos,
                CAST(nivel_taric AS INTEGER) AS nivel_taric,
                CAST(cod_taric AS DOUBLE) AS cod_taric,
                {estado} AS estado,
                {year} AS anio,
                {month} AS mes
            FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16')
            """
        else:  # sectores
            query = f"""
            SELECT
                CASE flujo WHEN 'E' THEN 1 WHEN 'I' THEN 0 ELSE NULL END AS flujo,
                CAST(pais AS INTEGER) AS pais,
                CAST(provincia AS INTEGER) AS provincia,
                CAST(nivel_sector_economico AS INTEGER) AS nivel_sector_economico,
                cod_sector_economico,
                CAST(REPLACE(euros, ',', '.') AS DOUBLE) AS euros,
                CAST(REPLACE(dolares, ',', '.') AS DOUBLE) AS dolares,
                {estado} AS estado,
                {year} AS anio,
                {month} AS mes
            FROM read_csv_auto('{csv_file}', delim='\\t', encoding='utf-16',
                             types={{'cod_sector_economico': 'VARCHAR'}})
            """
        
        return self.duckdb_con.execute(query).pl()

    def aggregate_spain(self, df: pl.DataFrame, nivel: str) -> pl.DataFrame:
        """Agrega datos por España (suma de todas las provincias)."""
        
        if nivel == "taric":
            group_cols = ["flujo", "pais", "nivel_taric", "cod_taric", "estado", "anio", "mes"]
            agg_cols = ["euros", "dolares", "kilogramos"]
        else:  # sectores
            group_cols = ["flujo", "pais", "nivel_sector_economico", "cod_sector_economico", 
                         "estado", "anio", "mes"]
            agg_cols = ["euros", "dolares"]
        
        return (
            df
            .group_by(group_cols)
            .agg([pl.col(col).sum() for col in agg_cols])
        )

    def process_ccaa_totals(
        self, 
        df: pl.DataFrame, 
        estado: int, 
        year: int, 
        month: int
    ) -> pl.DataFrame:

        mapeo_ccaa = [
            0, 14, 7, 13, 1, 6, 9, 4, 8, 6, 9, 1, 13, 7, 1, 10,
            7, 8, 1, 7, 14, 1, 2, 1, 6, 8, 16, 10, 15, 1, 11, 12,
            10, 3, 6, 5, 10, 6, 5, 17, 6, 1, 6, 8, 2, 7, 13, 6,
            14, 6, 2, 18, 19
        ]

        mapping = {prov: mapeo_ccaa[prov] for prov in range(1, len(mapeo_ccaa))}

        df_nivel1 = (
            df
            .filter(pl.col("nivel_sector_economico") == 1)
            .with_columns(
                pl.col("provincia")
                .replace(mapping, default=0)
                .cast(pl.Int32)
                .alias("cod_comunidad")
            )
            .filter(pl.col("cod_comunidad") >= 0)
        )

        # Totales por CCAA
        df_ccaa = (
            df_nivel1
            .group_by(["flujo", "cod_comunidad", "estado", "anio", "mes"])
            .agg([
                pl.col("euros").sum(),
                pl.col("dolares").sum()
            ])
        )

        # Totales nacionales
        df_nacional = (
            df_nivel1
            .group_by(["flujo", "estado", "anio", "mes"])
            .agg([
                pl.col("euros").sum(),
                pl.col("dolares").sum()
            ])
            .with_columns(
                pl.lit(99, dtype=pl.Int32).alias("cod_comunidad")
            )
        )

        cols = ["flujo", "cod_comunidad", "estado", "anio", "mes", "euros", "dolares"]

        return (
            pl.concat([
                df_ccaa.select(cols),
                df_nacional.select(cols)
            ])
            .sort(["flujo", "cod_comunidad"])
        )

    # ==================== PROCESAMIENTO DE RANGOS ====================
    def process_range(
        self,
        year_start: int,
        year_end: int,
        version: str = "def",
        ultimo_mes: int | None = None,
        raw_base_dir: str | Path = "data/raw",
        filtros_provincia: dict[str, list[int]] | None = None,
        skip_existing: bool = False
    ) -> dict[str, int]:
        """
        Procesa un rango de años completo.
        
        Args:
            year_start: Año inicial (inclusivo)
            year_end: Año final (inclusivo)
            version: 'def' o 'prov'
            ultimo_mes: Si se especifica, solo procesa hasta este mes en year_end
            raw_base_dir: Directorio de CSVs
            filtros_provincia: Filtros de provincia
            skip_existing: Si True, salta archivos ya procesados
            
        Returns:
            Dict con estadísticas: {'procesados': N, 'saltados': N, 'errores': N}
        """
        stats = {'procesados': 0, 'saltados': 0, 'errores': 0}
        
        self.logger.info(f"Procesando rango {year_start}-{year_end} ({version})")
        start_time = datetime.now()
        
        for year in range(year_start, year_end + 1):
            if year == year_end and ultimo_mes is not None:
                meses = range(1, ultimo_mes + 1)
            else:
                meses = range(1, 13)
            
            for month in meses:
                # Verificar si ya existe
                if skip_existing and self.exists("", "taric", 
                                                 1 if version == "def" else 0, 
                                                 year, month):
                    self.logger.debug(f"Saltando {year}-{month:02d} (ya existe)")
                    stats['saltados'] += 1
                    continue
                
                try:
                    self.csv_to_parquet(
                        year, month, version, raw_base_dir, filtros_provincia
                    )
                    stats['procesados'] += 1
                except FileNotFoundError as e:
                    self.logger.debug(f"Archivo no encontrado: {year}-{month:02d}")
                    stats['saltados'] += 1
                except Exception as e:
                    self.logger.error(f"Error procesando {year}-{month:02d}: {e}")
                    stats['errores'] += 1
        
        elapsed = datetime.now() - start_time
        self.logger.info(
            f"Completado en {elapsed}. "
            f"Procesados: {stats['procesados']}, "
            f"Saltados: {stats['saltados']}, "
            f"Errores: {stats['errores']}"
        )
        
        return stats

    # ==================== DOMINIOS DERIVADOS ====================

    def process_derived_domain(
        self,
        *,
        ambito: str,
        dominio_in: str,
        dominio_out: str,
        estado: int,
        anio: int,
        mes: int,
        transform: Callable[[pl.DataFrame], pl.DataFrame],
        skip_if_exists: bool = False
    ) -> None:
        """
        Procesa un dominio derivado a partir de datos base.
        
        Args:
            ambito: Ámbito geográfico (espana, madrid, etc.)
            dominio_in: Dominio de entrada (taric, sectores)
            dominio_out: Dominio de salida (euros_taric, kg_pais, etc.)
            estado: Estado (0=prov, 1=def)
            anio: Año
            mes: Mes
            transform: Función de transformación
            skip_if_exists: Si True, salta si el archivo ya existe
        """
        if skip_if_exists and self.exists(ambito, dominio_out, estado, anio, mes):
            self.logger.debug(
                f"Saltando {ambito}/{dominio_out} "
                f"{anio}-{mes:02d} (ya existe)"
            )
            return
        
        self.logger.debug(
            f"Procesando {ambito}/{dominio_in} → {dominio_out} "
            f"({anio}-{mes:02d})"
        )
        
        df_in = self.read(ambito, dominio_in, estado, anio, mes)
        df_out = transform(df_in)
        self.write(df_out, ambito, dominio_out, estado, anio, mes)

    def process_derived_pipeline(
        self,
        pipeline: list[tuple[str, str, Callable]],
        ambitos: list[str],
        year_start: int,
        year_end: int,
        estado: int,
        ultimo_mes: int | None = None,
        skip_if_exists: bool = True
    ) -> dict[str, int]:
        """
        Ejecuta un pipeline completo de dominios derivados.
        
        Args:
            pipeline: Lista de (dominio_in, dominio_out, transform)
            ambitos: Lista de ámbitos a procesar
            year_start: Año inicial
            year_end: Año final
            estado: Estado (0=prov, 1=def)
            ultimo_mes: Último mes a procesar en year_end
            skip_if_exists: Saltar archivos existentes
            
        Returns:
            Dict con estadísticas
        """
        stats = {'procesados': 0, 'saltados': 0, 'errores': 0}
        
        self.logger.info(
            f"Ejecutando pipeline: {len(pipeline)} transformaciones × "
            f"{len(ambitos)} ámbitos × {year_end - year_start + 1} años"
        )
        start_time = datetime.now()
        
        for ambito in ambitos:
            for dominio_in, dominio_out, transform in pipeline:
                for anio in range(year_start, year_end + 1):
                    meses = self.available_months(anio, year_end, ultimo_mes)
                    
                    for mes in meses:
                        try:
                            self.process_derived_domain(
                                ambito=ambito,
                                dominio_in=dominio_in,
                                dominio_out=dominio_out,
                                estado=estado,
                                anio=anio,
                                mes=mes,
                                transform=transform,
                                skip_if_exists=skip_if_exists
                            )
                            stats['procesados'] += 1
                        except FileNotFoundError:
                            stats['saltados'] += 1
                        except Exception as e:
                            self.logger.error(
                                f"Error en {ambito}/{dominio_out} "
                                f"{anio}-{mes:02d}: {e}"
                            )
                            stats['errores'] += 1
        
        elapsed = datetime.now() - start_time
        self.logger.info(
            f"Pipeline completado en {elapsed}. "
            f"Procesados: {stats['procesados']}, "
            f"Saltados: {stats['saltados']}, "
            f"Errores: {stats['errores']}"
        )
        
        return stats
    
    def process_month_pipeline(
        self,
        year: int,
        month: int,
        estado: int,
        pipeline: list[tuple[str, str, Callable]],
        ambitos: list[str],
        skip_if_exists: bool = False
    ) -> dict[str, int]:
        """
        Procesa un mes específico con todo el pipeline de dominios derivados.
        
        Args:
            year: Año
            month: Mes (1-12)
            estado: Estado (0=prov, 1=def)
            pipeline: Lista de (dominio_in, dominio_out, transform)
            ambitos: Lista de ámbitos a procesar
            skip_if_exists: Saltar archivos existentes
            
        Returns:
            Dict con estadísticas: {'procesados': N, 'saltados': N, 'errores': N}
        """
        stats = {'procesados': 0, 'saltados': 0, 'errores': 0}
        
        version = "def" if estado == 1 else "prov"
        self.logger.info(f"{'='*60}")
        self.logger.info(f"Procesando pipeline mes: {year}-{month:02d} ({version})")
        self.logger.info(f"{'='*60}")
        self.logger.info(f"Pipeline: {len(pipeline)} transformaciones")
        self.logger.info(f"Ámbitos: {', '.join(ambitos)}")
        
        start_time = datetime.now()
        
        for ambito in ambitos:
            self.logger.info(f"\n📍 Ámbito: {ambito}")
            
            for dominio_in, dominio_out, transform in pipeline:
                try:
                    self.process_derived_domain(
                        ambito=ambito,
                        dominio_in=dominio_in,
                        dominio_out=dominio_out,
                        estado=estado,
                        anio=year,
                        mes=month,
                        transform=transform,
                        skip_if_exists=skip_if_exists
                    )
                    stats['procesados'] += 1
                    self.logger.debug(f"   ✓ {dominio_in} → {dominio_out}")
                except FileNotFoundError:
                    stats['saltados'] += 1
                    self.logger.debug(f"   ⊘ {dominio_in} no encontrado")
                except Exception as e:
                    self.logger.error(f"   ✗ Error {dominio_in} → {dominio_out}: {e}")
                    stats['errores'] += 1
        
        elapsed = datetime.now() - start_time
        
        self.logger.info(f"\n{'='*60}")
        self.logger.info(f"Completado en {elapsed}")
        self.logger.info(f"Procesados: {stats['procesados']}, "
                        f"Saltados: {stats['saltados']}, "
                        f"Errores: {stats['errores']}")
        
        if stats['errores'] == 0:
            self.logger.info("✅ Mes procesado sin errores")
        else:
            self.logger.warning(f"⚠️  Mes procesado con {stats['errores']} errores")
        
        return stats
    
    def merge_parquet_range(
        self,
        *,
        ambito: Literal["espana", "madrid", ""],
        dominio: str,
        anio_inicio: int,
        anio_definitivo: int,
        anio_fin: int,
        meses: list[int] | None = None,
        filename: str = "data_0.parquet",
        lazy: bool = True,
    ) -> pl.DataFrame | pl.LazyFrame:
        """
        Junta múltiples parquets de un dominio y ámbito en un único DataFrame.

        Reglas de estado:
        - anio <= anio_definitivo → estado = 1 (definitivo)
        - anio >  anio_definitivo → estado = 0 (provisional)
        
        Args:
            anio_definitivo: Año hasta el cual los datos son definitivos.
                            Puede ser anterior, igual o posterior al rango.
        """

        if anio_inicio > anio_fin:
            raise ValueError("anio_inicio no puede ser mayor que anio_fin")

        if meses is None:
            meses = list(range(1, 13))
        else:
            meses = _validate_meses(meses)

        self.logger.info(
            f"Agregando {ambito or 'nacional'}/{dominio} "
            f"{anio_inicio}-{anio_fin} "
            f"(def hasta {anio_definitivo})"
        )

        lazy_frames: list[pl.LazyFrame] = []
        total_archivos = 0

        for anio in range(anio_inicio, anio_fin + 1):
            # Determinar estado según año definitivo
            estado = 1 if anio <= anio_definitivo else 0

            for mes in meses:
                if not self.exists(
                    ambito, dominio, estado, anio, mes, filename
                ):
                    self.logger.debug(
                        f"⊘ No existe: {ambito}/{dominio} "
                        f"estado={estado} {anio}-{mes:02d}"
                    )
                    continue

                lf = self.scan(
                    ambito=ambito,
                    dominio=dominio,
                    estado=estado,
                    anio=anio,
                    mes=mes,
                    filename=filename,
                )

                # Seguridad: forzamos columnas temporales si no vienen
                lf = lf.with_columns(
                    pl.lit(estado).alias("estado"),
                    pl.lit(anio).alias("anio"),
                    pl.lit(mes).alias("mes"),
                )

                lazy_frames.append(lf)
                total_archivos += 1

        if not lazy_frames:
            raise RuntimeError(
                f"No se encontró ningún parquet para "
                f"{ambito}/{dominio}"
            )

        self.logger.info(
            f"✓ {total_archivos} parquets añadidos a la agregación"
        )

        result = pl.concat(lazy_frames, how="vertical")

        return result if lazy else result.collect()

    # ==================== UTILIDADES ====================

    @staticmethod
    def available_months(
        anio: int, 
        ultimo_anio_prov: int, 
        ultimo_mes_disponible: int | None
    ) -> range:
        """
        Devuelve el rango de meses disponibles para un año dado.
        
        Args:
            anio: Año a consultar
            ultimo_anio_prov: Último año provisional con datos incompletos
            ultimo_mes_disponible: Último mes disponible en ese año provisional
        
        Returns:
            Range de meses disponibles
        """
        if anio == ultimo_anio_prov and ultimo_mes_disponible is not None:
            return range(1, ultimo_mes_disponible + 1)
        return range(1, 13)


# ==================== FUNCIONES DE AGREGACIÓN ====================

def aggregate_domain(
    df: pl.DataFrame,
    group_cols: Sequence[str],
    value_col: str,
) -> pl.DataFrame:
    """Agregación genérica por columnas de grupo."""
    return df.group_by(group_cols).agg(pl.col(value_col).sum())


# TARIC - Euros
def euros_taric(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(df, ["flujo", "nivel_taric", "cod_taric"], "euros")

def euros_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df.filter(pl.col("nivel_sector_economico") == 1),
        ["flujo", "pais"],
        "euros"
    )

def euros_taric_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(df, ["flujo", "nivel_taric", "cod_taric", "pais"], "euros")


def euros_taric_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(df, ["flujo", "nivel_taric", "cod_taric", "pais"], "euros")

# TARIC - Kilogramos
def kg_taric(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(df, ["flujo", "nivel_taric", "cod_taric"], "kilogramos")

def kg_pais(df: pl.DataFrame) -> pl.DataFrame:
        return aggregate_domain(
        df.filter(pl.col("nivel_taric") == 1),
        ["flujo", "pais"],
        "kilogramos"
    )

def kg_taric_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(df, ["flujo", "nivel_taric", "cod_taric", "pais"], "kilogramos")


# SECTORES - Euros
def euros_sectores(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df, 
        ["flujo", "nivel_sector_economico", "cod_sector_economico"], 
        "euros"
    )

def euros_sectores_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_sector_economico", "cod_sector_economico", "pais"],
        "euros"
    )