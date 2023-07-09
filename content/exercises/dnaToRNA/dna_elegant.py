from __future__ import annotations


def to_rna(dna: str) -> tuple[str, bool]:
    translation = {"G": "C", "C": "G", "T": "A", "A": "U"}
    result = [translation.get(c, f"err:{c}") for c in dna]
    errors = [err for err in result if err.startswith("err:")]
    if errors:
        return (errors[0][4:], False)
    return ("".join(result), True)
