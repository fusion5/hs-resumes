# Changelog for `hs-resume`

TODO: not sure if contextSummary: headingContext: is defined properly at the moment. It gets
passed as a context but the prompts specify: "Questo e il titolo della sezione Obiettivo". This 
seems stupid or broken

2026-04-23

Split into separate repository: this way the CV/LLM tools are separate from the contents

2026-02-16

Added model to cv.yaml file. This should prevent caching tests from failing in case of
a model model, and the model is also now configurable (it lets me use different models for 
italian/english).

2025-12-18

Add tests for caching
Add JSON coding/encoding for the large types.
Have separate programs.
intermediary steps to improve caching. Provide context definition file. Provide action definition file.

2025-12-15

Make context separate from atoms. Structure the tools differently.
