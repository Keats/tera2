import dis

text = """
if a:
    res = 1
elif b == 1:
    res = 2
else:
    res = 3
res += 1
"""

print(dis.dis(text))

"""
  0           0 RESUME                   0

  2           2 LOAD_NAME                0 (a)
              4 POP_JUMP_FORWARD_IF_FALSE     4 (to 14)

  3           6 LOAD_CONST               0 (1)
              8 STORE_NAME               1 (res)
             10 LOAD_CONST               3 (None)
             12 RETURN_VALUE

  4     >>   14 LOAD_NAME                2 (b)
             16 LOAD_CONST               0 (1)
             18 COMPARE_OP               2 (==)
             24 POP_JUMP_FORWARD_IF_FALSE     4 (to 34)

  5          26 LOAD_CONST               1 (2)
             28 STORE_NAME               1 (res)
             30 LOAD_CONST               3 (None)
             32 RETURN_VALUE

  7     >>   34 LOAD_CONST               2 (3)
             36 STORE_NAME               1 (res)
             38 LOAD_CONST               3 (None)
             40 RETURN_VALUE

"""