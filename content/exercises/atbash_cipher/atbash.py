from __future__ import annotations
import string


SWAP = {k: v for k, v in zip(string.ascii_lowercase, string.ascii_lowercase[::-1])}
ALPHANUMERIC = string.ascii_lowercase + string.digits

def encode(plain: str) -> str:
    stripped = plain.lower()
    encode_no_spaces = ''.join([SWAP.get(c, c) for c in stripped if c in ALPHANUMERIC])
    return " ".join([encode_no_spaces[i:i+5] for i in range(0, len(encode_no_spaces), 5)])

def decode(encoded: str) -> str:
    return ''.join([SWAP.get(c, c) for c in encoded if c in ALPHANUMERIC])
    