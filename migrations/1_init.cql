use clicklac;

create table user_account (
 id uuid primary key,
 user_name text, -- unique
 name text, 
 email text, -- unique
 password text,
 bio text,
 url text,
 avatar_url text,
 location text,
 activated_at timestamp,
 suspended_at timestamp,
 last_login_at timestamp,
 created_at timestamp,
 updated_at timestamp,
 state text -- (active, suspended, locked-out)
);


create table login_by_email (
 email text, -- unique
 user_id uuid,
 primary key(email)
) with comment = 'for logging in and account creation/update';


create table login_by_user_name (
 user_name text, -- unique
 user_id uuid, 
 primary key(user_name)
) with comment = 'for logging in and account creation/update';

create table user_session (
 user_id uuid,
 session_id text,
 start_at timestamp,
 end_at timestamp,
 host text, -- machine from where the session was initiated
 remote_ip text, -- ip of the http client
 primary key(user_id, session_id)
) with comment = 'to see all sessions of a user';


create table session (
 session_id text primary key,
 user_id uuid,
 start_at timestamp,
 end_at timestamp,
 host text, -- machine from where the session was initiated
 remote_ip text, -- ip of the http client
 serialized_value blob, -- store user session related info, keep this minimal, or use a map
) with comment = 'to get session info directly';

create table user_last_activity (
 user_id uuid primary key,
 session_id text,
 last_activity_at timestamp
) with compaction = {'class' : 'LeveledCompactionStrategy'};

