# jsown-utils

jsown-utils is utilities for Common Lisp JSON library [jsown](https://github.com/madnificent/jsown), to pretty print JSON and so on.

-----------------------------------------------------------------
## License

Copyright(c) 2017 Muyinliu Xing Released under the ISC License.

-----------------------------------------------------------------
## Dependencies

- [jsown](https://github.com/madnificent/jsown)

-----------------------------------------------------------------
## Install and load with QuickLisp

In shell:

```shell
git clone https://github.com/muyinliu/jsown-utils.git
cp -r jsown-utils ~/quicklisp/local-projects/jsown-utils
```

Then in Common Lisp:

```lisp
(ql:quickload 'jsown-utils)
```

-----------------------------------------------------------------
## Usage

### pretty print utils

#### `jsown:pprint-json`, pretty print JSON

```lisp
(jsown:pprint-json "{\"HeWeather5\":[{\"basic\":{\"city\":\"北京\",\"cnty\":\"中国\",\"id\":\"CN101010100\",\"lat\":\"39.904000\",\"lon\":\"116.391000\",\"update\":{\"loc\":\"2017-03-21 08:51\",\"utc\":\"2017-03-21 00:51\"}},\"now\":{\"cond\":{\"code\":\"101\",\"txt\":\"多云\"},\"fl\":\"0\",\"hum\":\"73\",\"pcpn\":\"0\",\"pres\":\"1027\",\"tmp\":\"6\",\"vis\":\"7\",\"wind\":{\"deg\":\"350\",\"dir\":\"东北风\",\"sc\":\"4-5\",\"spd\":\"18\"}},\"status\":\"ok\"}]}")
```
```=>
{
    "HeWeather5": [
        {
            "basic": {
                "city": "北京",
                "cnty": "中国",
                "id": "CN101010100",
                "lat": "39.904000",
                "lon": "116.391000",
                "update": {
                    "loc": "2017-03-21 08:51",
                    "utc": "2017-03-21 00:51"
                }
            },
            "now": {
                "cond": {
                    "code": "101",
                    "txt": "多云"
                },
                "fl": "0",
                "hum": "73",
                "pcpn": "0",
                "pres": "1027",
                "tmp": "6",
                "vis": "7",
                "wind": {
                    "deg": "350",
                    "dir": "东北风",
                    "sc": "4-5",
                    "spd": "18"
                }
            },
            "status": "ok"
        }
    ]
}
; No value
```

#### `jsown:pretty-json`, pretty JSON

Make JSON much more readable by add some newline or indent.

```lisp
(jsown:pretty-json "{\"HeWeather5\":[{\"basic\":{\"city\":\"北京\",\"cnty\":\"中国\",\"id\":\"CN101010100\",\"lat\":\"39.904000\",\"lon\":\"116.391000\",\"update\":{\"loc\":\"2017-03-21 08:51\",\"utc\":\"2017-03-21 00:51\"}},\"now\":{\"cond\":{\"code\":\"101\",\"txt\":\"多云\"},\"fl\":\"0\",\"hum\":\"73\",\"pcpn\":\"0\",\"pres\":\"1027\",\"tmp\":\"6\",\"vis\":\"7\",\"wind\":{\"deg\":\"350\",\"dir\":\"东北风\",\"sc\":\"4-5\",\"spd\":\"18\"}},\"status\":\"ok\"}]}")
```
```=>
"{
    \"HeWeather5\": [
        {
            \"basic\": {
                \"city\": \"北京\",
                \"cnty\": \"中国\",
                \"id\": \"CN101010100\",
                \"lat\": \"39.904000\",
                \"lon\": \"116.391000\",
                \"update\": {
                    \"loc\": \"2017-03-21 08:51\",
                    \"utc\": \"2017-03-21 00:51\"
                }
            },
            \"now\": {
                \"cond\": {
                    \"code\": \"101\",
                    \"txt\": \"多云\"
                },
                \"fl\": \"0\",
                \"hum\": \"73\",
                \"pcpn\": \"0\",
                \"pres\": \"1027\",
                \"tmp\": \"6\",
                \"vis\": \"7\",
                \"wind\": {
                    \"deg\": \"350\",
                    \"dir\": \"东北风\",
                    \"sc\": \"4-5\",
                    \"spd\": \"18\"
                }
            },
            \"status\": \"ok\"
        }
    ]
}"
```

### accessor utils

#### `jsown:json-val`, get value from JSON with key

```lisp
(jsown:json-val (jsown:parse "{\"key1\":\"value1\",\"key2\":2}") "key1")
```
=>
```=>
"value1"
```

#### `jsown:json-vals`, get value from JSON object with keys(JavaScript style)

```lisp
(jsown:json-vals (jsown:parse "{\"key1\":{\"key1.1\":\"value1.1\"},\"key2\":2}") 
                     "key1" "key1.1")
```
=>
```=>
"value1.1"
```

#### `jsown:json-update-in`, update value of JSON object with keys(JavaScript style)

```lisp
(defvar *json* (jsown:parse "{\"key1\":{\"key1.1\":\"value1.1\"},\"key2\":2}"))
(jsown:json-update-in *json* ("key1" "key1.1") "new-value1.1")
```
=>
```=>
"new-value1.1"
```

```lisp
*json*
```
=>
```=>
(:OBJ ("key1" :OBJ ("key1.1" . "new-value1.1")) ("key2" . 2))
```

-----------------------------------------------------------------
## More

Welcome to reply.
