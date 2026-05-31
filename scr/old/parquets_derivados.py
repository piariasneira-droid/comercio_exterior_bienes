import polars as pl
from typing import Callable, Sequence
from .parquet_store import ParquetStore

def aggregate_domain(
    df: pl.DataFrame,
    group_cols: Sequence[str],
    value_col: str,
) -> pl.DataFrame:
    return (
        df
        .group_by(group_cols)
        .agg(pl.col(value_col).sum())
    )


def euros_taric(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_taric", "cod_taric"],
        "euros",
    )


def euros_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "pais"],
        "euros",
    )

def euros_taric_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_taric", "cod_taric", "pais"],
        "euros",
    )


def kg_taric(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_taric", "cod_taric"],
        "kilogramos",
    )


def kg_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "pais"],
        "kilogramos",
    )

def kg_taric_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_taric", "cod_taric", "pais"],
        "kilogramos",
    )


def euros_sectores(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_sector_economico", "cod_sector_economico"],
        "euros",
    )

def euros_sectores_pais(df: pl.DataFrame) -> pl.DataFrame:
    return aggregate_domain(
        df,
        ["flujo", "nivel_sector_economico", "cod_sector_economico", "pais"],
        "euros",
    )

def process_month(
    store: ParquetStore,
    *,
    ambito: str,
    dominio_in: str,
    dominio_out: str,
    estado: int,
    anio: int,
    mes: int,
    transform: Callable[[pl.DataFrame], pl.DataFrame],
) -> None:
    df_in = store.read(
        ambito=ambito,
        dominio=dominio_in,
        estado=estado,
        anio=anio,
        mes=mes,
    )

    df_out = transform(df_in)

    store.write(
        df=df_out,
        ambito=ambito,
        dominio=dominio_out,
        estado=estado,
        anio=anio,
        mes=mes,
    )

def meses_disponibles(anio: int, ultimo_anio_prov: int, ultimo_mes_disponible: int) -> range:
    """
    Devuelve el rango de meses disponibles para un año dado.
    
    Args:
        anio: Año a consultar.
        ultimo_anio_prov: Último año provisional con datos incompletos.
        ultimo_mes_disponible: Último mes disponible en ese año provisional.
    
    Retorna:
        Un range de meses disponibles para el año.
        Para el último año provisional, solo hasta ultimo_mes_disponible.
        Para los demás, todos los meses (1-12)
    """
    if anio == ultimo_anio_prov:
        return range(1, ultimo_mes_disponible + 1)  # +1 porque range excluye el límite
    else:
        return range(1, 13)
