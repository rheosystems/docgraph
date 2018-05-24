BEGIN;
SET client_min_messages = 'warning';
SET SEARCH_PATH=docgraph;

INSERT INTO users (useremail, username, userpassword) VALUES
  ('mikkel@gmail.com'    , 'mschr'   , 'secret'),
  ('handsome@gmail.com'  , 'handsome', 'secret'),
  ('kego@gmail.com'      , 'kego'    , 'secret'),
  ('dima@braingiga.co.za', 'dima'    , 'secret');

INSERT INTO projects (reference, name) VALUES
  ('docgraph' , 'DocGraph'),
  ('braingiga', 'BrainGiga') ;

INSERT INTO project_users (user_email, project_reference) VALUES
  ('mikkel@gmail.com'    , 'docgraph'),
  ('handsome@gmail.com'  , 'docgraph'),
  ('kego@gmail.com'      , 'docgraph'),
  ('dima@braingiga.co.za', 'braingiga');

INSERT INTO documents (reference, title, version, owner) VALUES
  ('aima', 'Artificial Intelligence - A modern approach'      , 'v2'         , 'mikkel@gmail.com'),
  ('paip', 'Paradigms of Artificial Intelligence Programming' , ''           , 'mikkel@gmail.com'),
  ('sicp', 'Structure and Interpretation of Computer Programs', '2nd Edition', 'mikkel@gmail.com');

COMMIT;
