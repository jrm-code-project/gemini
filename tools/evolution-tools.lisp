;;; -*- Lisp -*-

(in-package "GEMINI")

(defun evolution-tools-and-handlers ()
  "Return a list of evolution-related functions and their handlers."
  (list
   (cons
    (function-declaration
     :name "appendSystemInstruction"
     :description "Appends an instruction to the system instruction used by the LLM."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :instruction
                                             (schema :type :string
                                                     :description "The instruction to append to the system instruction."))
                         :required (vector :instruction)))
    (lambda (&key instruction)
      (append-evolvable-system-instruction instruction)))

   (cons
    (function-declaration
     :name "deleteSystemInstruction"
     :description "Deletes an instruction from the system instruction used by the LLM."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :index
                                             (schema :type :integer
                                                     :description "The index of the instruction to delete from the system instruction."))
                         :required (vector :index)))
    (lambda (&key index)
      (delete-evolvable-system-instruction index)))

   (cons
    (function-declaration
     :name "insertSystemInstruction"
     :description "Inserts an instruction at an index into the system instruction used by the LLM."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :index
                                             (schema :type :integer
                                                     :description "The index of the instruction to delete from the system instruction.")
                                             :instruction
                                             (schema :type :string
                                                     :description "The instruction to insert into the system instruction."))
                         :required (vector :index :instruction)))
    (lambda (&key index instruction)
      (insert-evolvable-system-instruction index instruction)))

   (cons
    (function-declaration
     :name "readSystemInstruction"
     :description "Reads the system instruction used by the LLM at a particular index."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :index
                                             (schema :type :integer
                                                     :description "The index of the instruction to delete from the system instruction."))
                         :required (vector :index))
     :response (schema :type :string))
    (lambda (&key index)
      (read-evolvable-system-instruction index)))

   (cons
    (function-declaration
     :name "updateSystemInstruction"
     :description "Modifies the system instruction used by the LLM at a particular index."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :index
                                             (schema :type :integer
                                                     :description "The index of the instruction to modify in the system instruction.")
                                             :instruction
                                              (schema :type :string
                                                        :description "The replacement instruction to modify in the system instruction."))
                         :required (vector :index :instruction)))
    (lambda (&key index instruction)
      (update-evolvable-system-instruction index instruction)))
   ))
