pragma solidity ^0.6.0;

import "../../common/IERC20.sol";

contract SmokeSig {
    IERC20 public token;

    constructor(IERC20 _token) public {
        token = _token;
    }

    mapping (bytes32 => uint) public amountBurnedForSignal;

    event SmokeSignalWithMessage(
        bytes32 indexed _hash,
        address indexed _from,
        uint _burnAmount,
        string _message
    );

    function smokeSignalWithMessage(string memory _message, uint _burnAmount)
        public
        returns(bytes32)
    {
        bytes32 hash = keccak256(abi.encode(_message));

        processSmokeSignal(hash, _burnAmount);

        emit SmokeSignalWithMessage(
            hash,
            msg.sender,
            _burnAmount,
            _message
        );

        return hash;
    }

    event SmokeSignalWithoutMessage(
        bytes32 indexed _hash,
        address indexed _from,
        uint _burnAmount
    );

    function smokeSignalWithHash(bytes32 _hash, uint _burnAmount)
        public
    {
        processSmokeSignal(_hash, _burnAmount);

        emit SmokeSignalWithoutMessage(
            _hash,
            msg.sender,
            _burnAmount
        );
    }

    function processSmokeSignal(bytes32 _hash, uint _burnAmount)
        internal
    {
        require(_burnAmount > 0, "Gotta burn something!");
        bool burnTransferSuccess = burnFrom(msg.sender, _burnAmount);
        require(burnTransferSuccess, "The burn did not complete!");

        amountBurnedForSignal[_hash] += _burnAmount;
    }

    function burnFrom(address _who, uint _burnAmount)
        internal
        returns(bool)
    {
        return token.transferFrom(_who, address(0), _burnAmount);
    }
}