## Welcome

This is Clicklac API server's documentation.

All endpoints require authentication except 'POST /auth', and 'POST /user'.

Upon authentication via 'POST /auth', you get a session ID in the response. To protected APIs, you can send this id in the form of a cookie (_sid) or in 'Authorization' header.

After creating an account(POST /auth), you may use email or username as user ID to authenticate.

API root is /api/ or /api/v1/.

Enjoy!

## DELETE /auth

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 204
- Headers: [("Set-Cookie","_sid=TSHo3gQC4ew3dkPq15NMBRwy")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## POST /auth

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **X-Forwarded-For** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "userId": "foo@bar.com",
    "password": "mypass"
}
```

#### Response:

- Status code 201
- Headers: [("Set-Cookie","_sid=TSHo3gQC4ew3dkPq15NMBRwy")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"startAt":"2016-10-09T05:00:00Z","userId":"ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf","endAt":"2021-10-08T05:00:00Z","id":"TSHo3gQC4ew3dkPq15NMBRwy"}
```

## POST /click

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "text": "hi there"
}
```

#### Response:

- Status code 201
- Headers: [("Location","")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"clickId":21,"userId":"ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf","text":"hi there","likeCount":21,"favCount":3,"postedAt":"2016-10-09T05:00:00Z"}
```

## DELETE /click/:clickid

#### Authentication



Clients must supply the following data


#### Captures:

- *clickid*: (integer) click id

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## PUT /click/:clickid

#### Authentication



Clients must supply the following data


#### Captures:

- *clickid*: (integer) click id

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "favCount": 3,
    "text": "hi there",
    "likeCount": 21,
    "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
    "postedAt": "2016-10-09T05:00:00Z",
    "clickId": 21
}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /click/user/:userid

#### Authentication



Clients must supply the following data


#### Captures:

- *userid*: (UUID) user id

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    }
]
```

- 

```javascript
[
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    },
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    }
]
```

- 

```javascript
[
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    },
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    },
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    }
]
```

- 

```javascript
[
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    },
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    },
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    },
    {
        "favCount": 3,
        "text": "hi there",
        "likeCount": 21,
        "userId": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
        "postedAt": "2016-10-09T05:00:00Z",
        "clickId": 21
    }
]
```

## POST /user

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "bio": "some stuff",
    "email": "foo@bar.com",
    "avatarUrl": "http://aws/s3/avatar.png",
    "location": "San Jose, CA",
    "username": "onurz",
    "name": "Onur",
    "password": "",
    "webUrl": "http://www.foo.bar.com"
}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"bio":"some stuff","email":"foo@bar.com","avatarUrl":"http://aws/s3/avatar.png","createdAt":"2016-10-09T05:00:00Z","location":"San Jose, CA","activatedAt":"2016-10-09T05:00:00Z","url":"http://www.foo.bar.com","username":"onurz","name":"Onur","id":"ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf"}
```

## PUT /user

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "bio": "some stuff",
    "email": "foo@bar.com",
    "avatarUrl": "http://aws/s3/avatar.png",
    "location": "San Jose, CA",
    "username": "onurz",
    "name": "Onur",
    "webUrl": "http://www.foo.bar.com"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{
    "bio": "some stuff",
    "email": "foo@bar.com",
    "avatarUrl": "http://www.aws.com/s3/avatar.png",
    "location": "San Jose, CA",
    "username": "onurz",
    "name": "Onur",
    "webUrl": "http://www.foo.bar.com"
}
```

## GET /user/:userid

#### Authentication



Clients must supply the following data


#### Captures:

- *userid*: (UUID) user id

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
null
```

- 

```javascript
{
    "bio": "some stuff",
    "email": "foo@bar.com",
    "avatarUrl": "http://aws/s3/avatar.png",
    "createdAt": "2016-10-09T05:00:00Z",
    "location": "San Jose, CA",
    "uid": "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf",
    "url": "http://www.foo.bar.com",
    "username": "onurz",
    "name": "Onur",
    "updatedAt": null
}
```

## PUT /user/:userid/account/state

#### Authentication



Clients must supply the following data


#### Captures:

- *userid*: (UUID) user id

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "state": "active"
}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

