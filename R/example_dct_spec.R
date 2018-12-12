#' @rdname extract_dct_specs
#' @export
example_dct_spec <- "
---
id: attitude_xl678lqgq
label: Attitude
date: 2018-11-28
source:
  label: Reasoned Action Approach
  xdoi: ISBN
  spec: p. 22
defs:
  def: Attitude Def
  source:
    spec: pp. 11-12
measure_dev:
  instr: Use semantic differentials with root \"For me, [TARGET BEHAVIOR] is ...\" and a bidimensional scale where the right-most anchor expresses a beneficial or pleasant evaluation and the left-most anchor expresses the opposite undesirable/harmful/unpleasant evaluation.
manipulate_dev:
  instr: persuasive communication
  source:
    label: pers comm matrix
    xdoi: doi of article
measure_code:
  instr: Operationalisations that measure beneficial or pleasant aspects of the latent disposition or tendency to respond favourably versus unfavourably to [target behavior], for example using the semantic differentials good/bad, pleasant/unpleasant, wise/unwise.
  source: none
manipulate_code:
  instr: Code any presentation of a stimulus asserting that engaging in the target behavior will yield positive or negative consequences.
  source: none
aspect_code:
  instruction: Expressions of any consequences for oneself that one perceives to be caused by engaging in the target behavior.
  source: none
parent:
  id: intention_xl678lqgr
alters:
  id:
---
";
