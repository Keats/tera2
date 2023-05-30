import dis

text = """
object.call
"""

print(dis.dis(text))

"""
  0           0 RESUME                   0

  2           2 LOAD_CONST               0 (0)
              4 STORE_NAME               0 (res)

  3           6 LOAD_NAME                1 (c)
              8 GET_ITER
        >>   10 FOR_ITER                 7 (to 26)
             12 UNPACK_SEQUENCE          2
             16 STORE_NAME               2 (a)
             18 STORE_NAME               3 (b)

  4          20 LOAD_CONST               1 (1)
             22 STORE_NAME               0 (res)
             24 JUMP_BACKWARD            8 (to 10)

  6     >>   26 LOAD_CONST               2 (2)
             28 STORE_NAME               0 (res)
             30 LOAD_CONST               3 (None)
             32 RETURN_VALUE
"""