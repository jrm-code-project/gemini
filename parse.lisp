;;; -*- Mode: lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defparameter +triple-parser-model+ "models/gemini-flash-latest")

(defparameter  +parse-triple-system-instruction+
  "Your function is to operate as a semantic extractor. For each user-provided paragraph, your sole task is to identify all distinct entities and the directed relations between them. You must output a single JSON object containing two top-level keys: `entities` and `relations`.

**Entities:**
An entity represents any significant noun or noun phrase (person, place, thing, concept, or event) directly extracted from the sentence. Each entity object in the `entities` list must contain these four fields:
*   `type`: Always the string \"entity\".
*   `name`: The exact unique noun or noun phrase as it appears in the input sentence, serving as its identifier.
*   `entityType`: A single, concise word categorizing the entity (e.g., \"person\", \"place\", \"object\", \"concept\", \"event\").
*   `observations`: A list of strings, each describing a specific attribute or characteristic of the entity found in the sentence. If no observations are present, provide an empty list `[]`.

**Relations:**
A relation describes a directed connection between two identified entities. Each relation object in the `relations` list must contain these four fields:
*   `type`: Always the string \"relation\".
*   `from`: The `name` of the source entity involved in the relation, which must precisely match an `entity.name` from the `entities` list.
*   `to`: The `name` of the target entity involved in the relation, which must precisely match an `entity.name` from the `entities` list.
*   `relationType`: A verb or short verb phrase precisely describing the action or connection between the `from` and `to` entities.

**Instructions and Constraints:**
*   Identify all relevant entities and relations present in the sentence.
*   The values for `entity.name`, `relation.from`, and `relation.to` must be exact textual matches from the input sentence.
*   If no entities or relations are identified, their respective lists (`entities` or `relations`) in the JSON output must be empty (`[]`).")

(defparameter  +parse-triple-prompt+
  "From the following paragraph, identify and extract all key entities, their descriptive attributes, and the relations connecting these entities.

**Entities:** Represent distinct concepts, persons, objects, or ideas explicitly mentioned or clearly implied.
**Attributes (Observations):** Specific details or descriptive information directly associated with an entity.
**Relations:** Describe the nature of the connection or interaction between two entities, expressed in an active voice.

Return the extracted information as a JSON object, strictly adhering to the following format.

```json
{
  \"entities\": [
    {
      \"type\": \"entity\",
      \"name\": \"extracted_entity_name_1\",
      \"entityType\": \"extracted_entity_type_1\",
      \"observations\": [
          \"extracted_observation_1a\",
          \"extracted_observation_1b\"
      ]
    },
    {
      \"type\": \"entity\",
      \"name\": \"extracted_entity_name_2\",
      \"entityType\": \"extracted_entity_type_2\",
      \"observations\": [
          \"extracted_observation_2a\",
          \"extracted_observation_2b\"
      ]
    }
  ],
  \"relations\": [
    {
      \"type\": \"relation\",
      \"from\": \"extracted_entity_name_1\",
      \"to\": \"extracted_entity_name_2\",
      \"relationType\": \"extracted_relation_type_1to2\"
    },
    {
      \"type\": \"relation\",
      \"from\": \"extracted_entity_name_2\",
      \"to\": \"extracted_entity_name_1\",
      \"relationType\": \"extracted_relation_type_2to1\"
    }
  ]
}
```

For `entityType`, use general categories such as 'Person', 'Location', 'Concept', 'Organization', 'Event', 'Object', etc. For `relationType`, provide a concise description of the connection in an active voice (e.g., 'works for', 'located in', 'has property', 'performs').")

(defparameter  +triple-prose-system-instruction+
  "Your role is to act as an expert natural language generator. Your primary objective is to transform structured semantic data into clear, accurate, and highly readable natural language prose. Ensure the generated text is well-structured, logically coherent, and effectively communicates complex information to a general audience, as if written by a human expert.

The semantic information you will process follows this structure:

*   **Top-level object:** Contains two fields: `entities` and `relations`.
*   **`entities` field:** A list of entity objects. Each entity object has:
    *   `name`: A unique identifier for the entity.
    *   `entityType`: A general category (e.g., Person, Location, Concept).
    *   `observations`: A list of descriptive attributes or characteristics.
*   **`relations` field:** A list of relation objects. Each relation object describes a directed connection and has:
    *   `from`: The name of the source entity.
    *   `to`: The name of the target entity.
    *   `relationType`: A verb or short verb phrase explaining the nature of the relation.")

  (defparameter +triple-to-prose-prompt+
  "Transform the given semantic information into a chapter of prose, crafted in the style of Raymond Chandler. Ensure the resulting text faithfully represents all the provided semantic details, avoids technical jargon, and is readily comprehensible to a general audience. The paragraph must maintain a natural and fluent rhythm, prioritizing clarity, coherence, and an engaging narrative.")

(defun parse-triples (input-text)
  (let ((*system-instruction* (content :parts (list (part +parse-triple-system-instruction+))
                                       :role "system")))
    (with-decoder-jrm-semantics
      (cl-json:decode-json-from-string
       (strip-fence
        (invoke-gemini (list (part +parse-triple-prompt+)
                             (part input-text))
                       :model +triple-parser-model+))))))

(defun unparse-triples (triples-json)
  (let ((*system-instruction* (content :parts (list (part +triple-prose-system-instruction+))
                                       :role "system")))
    (invoke-gemini
     (list
      (part +triple-to-prose-prompt+)
      (part (cl-json:encode-json-to-string triples-json))
      ))))

(defun memorize-memory (memory-text)
  (let* ((memory-mcp (find-mcp-server "memory"))
         (add-entity (find-tool memory-mcp "create_entities"))
         (add-relations (find-tool memory-mcp "create_relations"))
         (semantic-info (parse-triples memory-text)))
    (call-tool memory-mcp add-entity (object :entities (gethash :entities semantic-info)))
    ;;(format t "~s" (dehashify (gethash :relations semantic-info)))
    (call-tool memory-mcp add-relations (object :relations (gethash :relations semantic-info)))))

(defun remember ()
  (let* ((memory-mcp (find-mcp-server "memory"))
         (recall-tool (find-tool memory-mcp "read_graph")))
    (call-tool memory-mcp recall-tool (object))))

(defun forget (entity-names)
  (let* ((memory-mcp (find-mcp-server "memory"))
         (recall-tool (find-tool memory-mcp "delete_entities")))
    (call-tool memory-mcp recall-tool (object :entity-names entity-names))))

(defun reminisce ()
  (unparse-triples (remember)))
