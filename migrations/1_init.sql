create table click (
 id bigserial primary key,
 user_id uuid not null,
 click_text text not null check(length (click_text) <= 200),
 like_count integer not null default 0, 
 favorite_count integer not null default 0,
 posted_at timestamptz not null default now()
);
