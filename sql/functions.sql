CREATE OR REPLACE FUNCTION insert_user(
     email    CITEXT,
     password TEXT,
     url      CITEXT
 ) RETURNS VOID AS $$
     INSERT INTO users (email, password, email_url)
     VALUES($1, crypt($2, gen_salt('bf')), url);
 $$ LANGUAGE SQL SECURITY DEFINER;

CREATE OR REPLACE FUNCTION auth_user(
     email_arg    CITEXT,
     password_arg TEXT
 ) RETURNS TABLE(
  user_id      INT,
  full_name    TEXT,
  email        TEXT,
  short_bio    TEXT,
  bio          TEXT,
  picture      TEXT,
  company_id   INT,
  company_name TEXT,
  admin        BOOL
)AS $$
     SELECT user_id, full_name, U.email, short_bio, bio, picture, company_id, name, admin
       FROM users U
       LEFT JOIN companies USING (company_id)
      WHERE U.email = email_arg and password = crypt(password_arg, password);
 $$ LANGUAGE SQL STABLE SECURITY DEFINER;

CREATE OR REPLACE FUNCTION change_password(
     user_id  INT,
     password TEXT
 ) RETURNS VOID AS $$
     UPDATE users SET password = crypt($2, gen_salt('bf')),
                      password_url = null
     WHERE user_id = $1;
 $$ LANGUAGE SQL SECURITY DEFINER;
