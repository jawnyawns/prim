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
        self.assertEqual([120], exec(source_code))

    def test_exec_factorial_with_define(self):
        source_code = """
        (define factorial
          (lambda (n)
            (if (= n 0)
              1
              (* n (factorial (- n 1)))
            )
          )
        )
        (factorial 5)
        """
        self.assertEqual(["<DEFINITION(S) ADDED>", 120], exec(source_code))

    def test_exec_if(self):
        source_code = """
        (if 
          false 1
          (and (< 1 2) (< 1 3)) 2
          3
        )
        """
        self.assertEqual([2], exec(source_code))
    
    def test_exec_math(self):
        source_code = "(+ 1 (* (- -2.1 3.14) 4))"
        self.assertEqual([-19.96], exec(source_code))
    
    def test_exec_string(self):
        source_code = """
        (
          (lambda (x) x)
          "i'm a raw string... here comes a backslash: \... here come a tab:	... and now an emoji: 🤓... and now some korean: 김치"
        )
        """
        self.assertEqual(
            ["i'm a raw string... here comes a backslash: \... here come a tab:	... and now an emoji: 🤓... and now some korean: 김치"],
            exec(source_code)
        )

    def test_exec_string_concat(self):
        self.assertEqual(["hello"], exec('(++ "hell" "o")'))

    def test_exec_list(self):
        self.assertEqual(
            [2],
            exec("(value (rest (:: 1 (:: 2 (:: 3 (list))))))")
        )
    
    def test_exec_multiple_expressions(self):
        source_code = """
        (value (rest (:: 1 (:: 2 (:: 3 (list))))))
        (if 
          false 1
          (and (< 1 2) (< 1 3)) 2
          3
        )
        """
        self.assertEqual([2, 2], exec(source_code))
