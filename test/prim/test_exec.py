from unittest import TestCase
from prim.exec import exec

class TestExec(TestCase):
    def test_exec_crash(self):
        with self.assertRaises(ZeroDivisionError):
            exec("(/ 1 0)")
    
    def test_exec_factorial(self):
        source_code = """
        (
          (lambda (factorial)
            (factorial factorial 5)
          )
          (lambda (factorial n)
            (if (= n 0)
              1
              (* n (factorial factorial (- n 1)))
            )
          )
        )
        """
        self.assertEqual(120, exec(source_code))
    
    def test_exec_if(self):
        source_code = """
        (if 
          false 1
          (and (< 1 2) (< 1 3)) 2
          3
        )
        """
        self.assertEqual(2, exec(source_code))
    
    def test_exec_math(self):
        source_code = "(+ 1 (* (- -2.1 3.14) 4))"
        self.assertEqual(-19.96, exec(source_code))
    
    def test_exec_string(self):
        source_code = """
        (
          (lambda (x) x)
          "i'm a raw string... here comes a backslash: \... here come a tab:	... and now an emoji: 🤓... and now some korean: 김치"
        )
        """
        self.assertEqual(
            "i'm a raw string... here comes a backslash: \... here come a tab:	... and now an emoji: 🤓... and now some korean: 김치",
            exec(source_code)
        )

    def test_exec_string_concat(self):
        self.assertEqual("hello", exec('(++ "hell" "o")'))

    def test_exec_list(self):
        self.assertEqual(
            2,
            exec("(value (rest (:: 1 (:: 2 (:: 3 (list))))))")
        )
