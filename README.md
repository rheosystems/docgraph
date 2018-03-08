# docgraph

Keeps changelogs and bidirectional traceability for documents.


## Functionality

### Create Project

Create a project with a reference number and a name.


### Delete Project

Deleting a project will unassociate any documents from the
project.


### Register Document

Registering a project requires a reference number, a version, optionally a location (e.g. a directory location or url), and keywords.

### Add Document Dependency

Requires a document reference no.

- Update document
- Delete document
- List document changes
- List dependent documents (children)
- List document dependencies (parents)
- Export graph
- Export Document Metadata
- Import graph
- Import Document Metadata

## Model

Project >-< Document
Document >-< Document


## Project

- Reference Number
- Project Name


### Document

- Reference Number
- Version
- Owner
- Location
- Key words
- Dependencies
