import dis
import ast

text = """
[1,2,3][0]
"""

print(ast.dump(ast.parse(text)))
print(dis.dis(text))

"""

  2           2 LOAD_NAME                0 (data)
              4 GET_ITER
        >>    6 FOR_ITER                 5 (to 18)
              8 UNPACK_SEQUENCE          2
             12 STORE_NAME               1 (key)
             14 STORE_NAME               2 (val)

  3          16 JUMP_BACKWARD            6 (to 6)

  2     >>   18 LOAD_CONST               0 (None)
             20 RETURN_VALUE

"""