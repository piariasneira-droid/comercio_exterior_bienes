from pathlib import Path
import polars as pl

class ParquetStore:
    """
    Capa de acceso a datos Parquet con estructura tipo data lake.
    """

    def __init__(self, base_dir: str | Path):
        self.base_dir = Path(base_dir)

    def build_path(
        self,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
    ) -> Path:
        return (
            self.base_dir
            / ambito
            / dominio
            / f"estado={estado}"
            / f"anio={anio}"
            / f"mes={mes}"
            / filename
        )

    def read(
        self,
        ambito: str,
        dominio: str,
        estado: int,
        anio: int,
        mes: int,
        filename: str = "data_0.parquet",
    ) -> pl.DataFrame:
        path = self.build_path(
            ambito, dominio, estado, anio, mes, filename
        )

        if not path.exists():
            raise FileNotFoundError(f"No existe: {path}")

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
        path = self.build_path(
            ambito, dominio, estado, anio, mes, filename
        )

        if not path.exists():
            raise FileNotFoundError(f"No existe: {path}")

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
        path = self.build_path(
            ambito, dominio, estado, anio, mes, filename
        )

        path.parent.mkdir(parents=True, exist_ok=True)

        if path.exists() and not overwrite:
            raise FileExistsError(f"Ya existe: {path}")

        df.write_parquet(path)

        return path