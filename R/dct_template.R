dct_template <- function(dctId,
                         addComments = TRUE) {

  if (addComments) {
    res <- paste0('# Decentralized Construct Taxonomy Template

This file is an empty Decentralized Construct Taxonomy (DCT) template. The DCT
specification is contained in the YAML document below (delimited by the two
lines with the three dashes). Before and after the YAML document, other content
can be specified. Content outside the YAML delimiters is
ignored when the DCT specification is read, making it possible to include
arbitrary additional information (such as this text).

In the YAML document, lines starting with a pound/hash symbol, preceded by zero
or more spaces, are considered comments and are ignored. Such comments are used
below to explain the different fields that can be specified in a DCT
specification. There are two important things to keep in mind when specifying
a DCT specification.

First, leading spaces are very important, and always have to be multiples of
two. These spaces specify the structure of the DCT specification.

Second, while not always necessary, it is wise to always place specifications
in double quotes, and within those quotes, use single quotes if quotes are
required.

DCT specifications contain metadata and up to eight named blocks:

- **Metadata**: The metadata consists of the identifier, a human-readable
label, and a source.

- **Definition**: The first block is named `definition` and contains the
formal definition of the construct. This definition describes, as
explicitly as possible, which aspects of the human psychology fall within
this construct, and which aspects fall without it. The definition is ideally
sufficiently comprehensive to allow distinguishing this construct from related
constructs and to provide clear directions for developing and coding
measurement instruments, manipulations, and aspects, the specific instructions
for which can be included in the following blocks.

- **Developing measurement instruments**: This block, named
`measure_dev`, specifies how to develop a measurement instrument for this
construct. Measurement instruments are defined as specifications of a
registration procedure, usually accompanied by one or more stimuli and/or
procedures. For example, a very simple measurement instrument can consist
of a question (a stimulus) and a Likert scale (a stimulus combined with
means to register a response). Psychological measurement instruments often
use combinations of stimuli and response registrations that are devised
such that the relevant construct has a central role in the processing of
the stimulus and the generation of the response. Variation in the
registered responses is assumed to reflect variation in the target
construct, for example between people or within people over time.

- **Coding measurement instruments**: This block, named `measure_code`,
specifies how to code existing measurement instruments as measurement
instruments that measure this construct. Such instructions are required
when conducting literature syntheses such as systematic reviews or
meta-analyses, because authors often use a variety of different names for
the constructs they measure, and these names are not always consistent with
the theoretical definitions of those constructs. This necessitates
interpretation of the measurement instruments using specified construct
definitions and the corresponding coding instructions.

- **Developing manipulations**: This block, named `manipulate_dev`, specifies
how to develop manipulations for this construct, that is, stimuli and/or
procedures that, when administered to a person, will influence the relevant
construct. When the specified construct is a determinant (i.e. the construct
theoretically predicts behavior), such a manipulation is often called a
Behavior Change Principle (such as a BCT or a method for behavior change).

- **Coding manipulations**: This block, named `manipulate_code`, specifies
how to code manipulations of this construct, such as can be used, for example,
in literature syntheses.

- **Developing aspects**: This block, named `aspect_dev`, specifies how to
"develop" the aspects within this construct. Aspects are defined as aspects
of people\'s psychology that are sufficiently specific to capture in media
such as text, images, audio or video. Because constructs are usually generic,
their content can sometimes vary between populations, and for determinants,
between target behaviors. Therefore, a procedure to generate the relevant
specific content (i.e. the aspects) is important to be able to measure or
manipulate the construct in a given population or for a given target
behavior. Therefore, these aspects often provide the content used in the
measurement instruments and/or manipulations.

- **Coding aspects**: This block, named `aspect_code`, specifies how to
code aspects that may belong to this construct. These instructions can
be used, for example, when coding qualitative data.

- **Relationships to other constructs**: This block, named `rel`, specifies
how this construct relates to other constructs. Each relationship consists of
two elements: the identifier of another DCT specification (i.e. the construct
to which the present construct relates) and a relationship type.

Not all blocks need to be specified; for example, if a theory only specifies
how to measure a construct, without any guidelines for manipulating it,
the two `manipulate_`  two blocks can be omitted; or if a theory does not
specify how a construct\'s content can be identified, the two `aspect_` blocks
can be omitted.

```
---
dct:
  version: ',
                utils::packageVersion('dct') ,'

  ### The identifier (id) is the unique code used to refer to this
  ### Decentralized Construct Taxonomy specification. This id is the
  ### "computer-readable name" for this DCT specification.
  id: ',
                dctId, '

  ### A label is the "human-readable name" for this DCT specification.
  label: ""

  ### The date at which this DCT specification was created, using a
  ### four-digit year, two-digit month, and two-digit day separated by dashes.
  date: ""

  ### The source of a construct is a reference to an external source,
  ### such as an article, book, report, meeting minutes, or other document.
  ### Sources are optional, but are important to embed a DCT in the literature
  ### and provide background information.
  source:

    ### A source also has an identifier, making it possible to refer to the
    ### same source multiple times to specify specific sections for different
    ### parts of the DCT specification.
    id: ""

    ### The label can be e.g. a reference to an article/book or a webpage name.
    label: ""

    ### An xdoi is an extended digital object identifier, allowing reference
    ### to a doi, isbn, or url.
    xdoi: ""

################################################################################

  definition:
    ### The definition of this construct according to the source.
    definition: ""

    ### If a source id is specified, information provided there but omitted
    ### here will be copied here. This makes it possible to efficiently specify,
    ### for example, the pages in an article or book. If another id is specified,
    ### another source is created with the information specified here. If no id
    ### is specified, but other fields are specified, it is assumed the source
    ### specified as main DCT source is referenced.
    source:
      id: ""
      spec: ""

################################################################################

### After having specified the main DCT information block and the definition of
### this construct, one to six instructions can be specified. These always
### consist of an instruction and optionally a source (see above).

################################################################################

  measure_dev:
    instruction: ""
    source:
      spec: ""

################################################################################

  measure_code:
    instruction: ""
    source:
      spec: ""

################################################################################

  manipulate_dev:
    instruction: ""
    source:
      spec: ""

################################################################################

  manipulate_code:
    instruction: ""
    source:
      spec: ""

################################################################################

  aspect_dev:
    instruction: ""
    source:
      spec: ""

################################################################################

  aspect_code:
    instruction: ""
    source:
      spec: ""

################################################################################

  ### Relationships to one or more other constructs. See the documentation for
  ### instructions on how to specify multiple relationships.
  rel:

    ### The identifier of the other construct
    id: ""

    ### The type of relationship, such as "causal_influences_positive",
    ### "causal_influences_negative", or "structural_part_of"
    type: ""

################################################################################
---
```

This is the end of this DCT template. Note that this template uses Markdown
(https://en.wikipedia.org/wiki/Markdown). This has a number of advantages.
First, Markdown is easily readable without having been rendered to a format that
includes markup/formatting. Second, Markdown is automatically rendered by GitLab,
GitHub, and the Open Science Framework wiki pages. Third, Markdown can be
rendered to a variety of other formats such as PDF, HTML, or word processor
formats such as the OpenDocument Format or Microsoft Word\'s Docx format. Such
conversions are possible with a variety of Free/Libre of Open Source Software
tools, such as R using the `rmarkdown` package and Pandoc, which is
automatically installed along with R Studio.

The triple backticks that delimit the YAML document (i.e. in addition to the
triple dash delimitation) mean that the YAML document should be considered code,
and as such, that it should be formatted in a fixed-width font and retaining all
spaces and line breaks.

');
  } else {
    res <- paste0('---
dct:
  version: ',
                  utils::packageVersion('dct') ,'
  id: ',
                  dctId, '
  label: ""
  date: ""
  source:
    id: ""
    label: ""
    xdoi: ""

################################################################################

  definition:
    definition: ""
    source:
      spec: ""

################################################################################

  measure_dev:
    instruction: ""
    source:
      spec: ""

################################################################################

  measure_code:
    instruction: ""
    source:
      spec: ""

################################################################################

  manipulate_dev:
    instruction: ""
    source:
      spec: ""

################################################################################

  manipulate_code:
    instruction: ""
    source:
      spec: ""

################################################################################

  aspect_dev:
    instruction: ""
    source:
      spec: ""

################################################################################

  aspect_code:
    instruction: ""
    source:
      spec: ""

################################################################################

  rel:
    id: ""
    type: ""

################################################################################
---
');
  }

  return(res);

}
