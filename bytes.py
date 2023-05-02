import dis

text = """
res = a and b
res2 = a or b
"""

print(dis.dis(text))

"""
  0           0 RESUME                   0

  2           2 LOAD_NAME                0 (a)
              4 JUMP_IF_FALSE_OR_POP     1 (to 8)
              6 LOAD_NAME                1 (b)
        >>    8 STORE_NAME               2 (res)

  3          10 LOAD_NAME                0 (a)
             12 JUMP_IF_TRUE_OR_POP      1 (to 16)
             14 LOAD_NAME                1 (b)
        >>   16 STORE_NAME               3 (res2)
             18 LOAD_CONST               0 (None)
             20 RETURN_VALUE

"""