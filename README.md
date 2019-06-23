# elm-ethereum-generator
Generate an [elm-ethereum](https://github.com/cmditch/elm-ethereum) binding for an Ethereum smart contract from it's ABI

Example usage:
`elm-ethereum-generator some-abi.json src/MyContract.elm`

## Changelog

## 3.0.0

- Updated to latest version of elm-ethereum and elm 0.19 
- Added support for dynamic types (bytes, string, list)

### 2.0.0

- Changed imports from Abi to AbiEncode and AbiDecode
- Removed unsupported types for the time being.
- De-pluralize "ies" in type alias names. Add "y" to end.
   Helpful for properly naming data returned from functions with multiple return values (structs/tuples).
- Manually entry of staticByte length required for time being. Need to majorly refactor type parsing.

### 1.1.0

- Change all occurences of Evm to Abi
- Add "Contracts." for generated module name
- De-pluralize "s" in type-alias names.
    (Though not handling "ies")