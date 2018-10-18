#### 2.3.0

* Fix #59: Make autodecoder support optional fields when missing from JSON
* Fix #61: Support object keys with JsonPath characters when using `Decode.dict`
* Fix #51: Add support for `Raw` decoder in object builders
* Added CultureInfo.InvariantCulture to all Encoder functions where it was possible (by @draganjovanovic1)

#### 2.2.0

* Re-add optional and optionalAt related to #51
* Various improvements for Primitive types improvements  (by @draganjovanovic1)
* Fix decoding of optional fields (by @eugene-g)

#### 2.1.0

* Fix nested object builder (ex: get.Optional.Field > get.Required.Field)
* Fix exception handling

#### 2.0.0

* Release stable

#### 2.0.0-beta-004

* Add Encoders for all the equivalent Decoders

#### 2.0.0-beta-003

* Make auto decoder safe by default

#### 2.0.0-beta-002

* Fix `Decode.decodeString` signature

#### 2.0.0-beta-001

* Mark `Encode.encode`, `Decode.decodeString`, `Decode.decodeValue` as obsoletes
* Support auto decoders and encoders
* Remove pipeline style for the decoders
* Add object builder style for the decoders
* Better error, by now tracking the path

#### 1.1.0

* Ensure that `field` `at` `optional` `optionalAt` works with object

#### 1.0.1

* A float from int works

#### 1.0.0

* Initial release
