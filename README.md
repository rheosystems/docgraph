# docgraph

Keeps changelogs and bidirectional traceability for documents.

## Quick Start

To install dependencies and run executable. Requires `stack`.

```
> stack setup
> stack build
> stack exec docgraph-exe
```

Open a browser and go to `http://localhost:3000`.


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

## User

A user has an email address as a natural key (Use as primary
key). Additionally full name.

A user can be part of many projects and a project has many
users. Therefore need a many-to-many table. See code below and note
that we are using a composite primary key.

```
Table: project_users
  user_email references users(email),
  project_reference references projects(reference),
  PrimaryKey (user_email, project_reference)

```

A user profile should show which projects the user is a member of.
A project profile should show the users that are members of the project.
