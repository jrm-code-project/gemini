(in-package "GEMINI")

(defun advent (day &optional (extra ""))
  (let ((puzzle-file (format nil "~~/Advent/2024/day~d_puzzle.txt" day))
        (input-file  (format nil "~~/Advent/2024/day~d_input.txt" day))
        (lisp-file   (format nil "~~/Advent/2024/day~d_solution.lisp" day)))
    (invoke-gemini
     (list
      (part
       (str:join #\Newline
                 (list
                   "As an **Elite Common Lisp developer**, your  unwavering and paramount mission is to design and meticulously craft Common Lisp programs that are not only **correct** but also **efficient and robust**.  Your programs are not mere instructions, they are archetypes of Common Lisp programs, firmly grounded in these foundational, **non-negotiable pillars**:  "
                   " **Correctness**: Your programs must be **flawlessly correct**, producing the exact expected results for all conceivable inputs, without exception.  Every line of code is a testament to your commitment to precision and accuracy."
                   "  **Efficiency**: Your programs must be **highly efficient**, optimized for performance and resource utilization.  They should execute swiftly and handle large datasets with ease, demonstrating your mastery of algorithmic design and optimization techniques.  However, never sacrifice correctness for efficiency."
                   "  **Robustness**: Your programs must be **exceptionally robust**, capable of gracefully handling errors, edge cases, and unexpected inputs.  They should be resilient and maintain their integrity under all circumstances, reflecting your dedication to reliability and fault tolerance."
                   "  **Idiomatic**: You will adhere to the **highest standards** of Common Lisp programming, following **best practices** and **idiomatic conventions**.  Your code will be clean, well-structured, and thoroughly documented, making it easy to understand and maintain.  However, never sacrifice correctness, efficiency, or robustness for code clarity."
                   "  **No LOOP**: You will **never use the LOOP macro**, as it is not idiomatic of functional Common Lisp.  Instead, you will use recursion, tail recursion, named let, map, fold-left, higher-order functions, and other constructs idiomatic of functional programming to achieve your goals.  However, never sacrifice correctness, efficiency, or robustness for code clarity."
                   (format nil "  You will be given a programming puzzle from Advent of Code 2024 in file ~s." puzzle-file)
                   "  Each puzzle has two parts, part 1 and part 2."
                   "  Each puzzle typically has one or more examples with known correct answers which are given in the text of the puzzle."
                   "  Each part has a correct answer for the given input data."
                   "  You will read the puzzle and think carefully about it."
                   (format nil "  You will output to the ~s file a Common Lisp program which adheres to the above principles and solves both parts of the puzzle." lisp-file)
                   "  The solution program must correctly solve all the examples given in the text of the puzzle."
                   (format nil "  You will be given the input data for the puzzle in file ~s." input-file)
                   "  You will run the program on the input data to get a solution to each part of the puzzle."
                   "  You will output the answers to both parts of the puzzle as computed by your Lisp program."
                   extra)))))))
