# 🧮 **State Counter (Proof Token)**

>Source file: `src/Contracts/Samples/StateCounter.hs`

This example smart contract demonstrates how to maintain authoritative application state by way of an auxiliary proof token. A proof token (or "admin token") is an NFT that remains at the script address at all times, and thus can be used to restrict valid application state to the datum of the particular UTxO on which it resides.

The contract is parameterized by the *`AssetClass`* of this proof token. This produces a unique script where at any given time, only a single specific UTxO locked at the script address (containing the token) will be spendable. For a validating transaction to succeed, a single new UTxO must be locked at the script containing the same token by which the contract is parameterized, as well as an updated state in its datum.

In this simple example, we use an *`Integer`* value in the datum to create a counter that increments with each spending transaction. The same pattern can be used in more sophisticated contracts involving arbitrarily complex application state and additional functionality.

We'll reuse the NFT minting policy from `src/Contracts/Samples/NFT.hs`. To initialize the counter, a user must first mint a token with this policy, then lock it at the script address with a counter (datum) of zero.

To increment the counter, the user then spends the UTxO containing the proof token.