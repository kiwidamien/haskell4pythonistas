from __future__ import annotations


def to_rna(dna: str) -> tuple[str, bool]:
    translation = {"G": "C", "C": "G", "T": "A", "A": "U"}
    result = []
    for char in dna:
        if char not in translation:
            return (char, False)
        result.append(translation[char])
    return ("".join(result), True)
