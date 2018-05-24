BEGIN;
SET client_min_messages = 'warning';
CREATE SCHEMA IF NOT EXISTS docgraph;
SET SEARCH_PATH=docgraph;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Start of table definitions
-- users table
CREATE TABLE users (
  useremail       CITEXT PRIMARY KEY,
  userfullnames   TEXT   NULL,
  username        CITEXT NOT NULL,
  userpassword    TEXT   NOT NULL
);

-- projects table
CREATE TABLE projects (
   reference  TEXT   PRIMARY KEY,
   name       TEXT   NOT NULL
);

CREATE TABLE project_users (
  user_email        CITEXT REFERENCES  users(useremail),
  project_reference CITEXT REFERENCES  projects(reference),
  PRIMARY KEY (user_email, project_reference)
);

CREATE TABLE documents (
  reference   CITEXT PRIMARY KEY,
  title       TEXT   NOT NULL,
  version     TEXT   NOT NULL,
  owner       CITEXT REFERENCES users(useremail),
  keywords    TEXT   NULL,
  url	      TEXT   NULL
);

CREATE TABLE project_documents (
  project_reference  CITEXT REFERENCES projects(reference),
  document_reference CITEXT REFERENCES documents(reference),
  PRIMARY KEY (project_reference, document_reference)
);

-- Creating the `docgraph` user with the required permissions.
-- Must happen after all tables, views, etc. have been created.
DROP ROLE IF EXISTS docgraph;
CREATE ROLE docgraph WITH LOGIN ENCRYPTED PASSWORD 'docgraph';
ALTER ROLE docgraph SET search_path TO docgraph;
GRANT USAGE ON SCHEMA docgraph TO docgraph;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA docgraph TO docgraph;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA docgraph TO docgraph;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA docgraph TO docgraph;

COMMIT;
