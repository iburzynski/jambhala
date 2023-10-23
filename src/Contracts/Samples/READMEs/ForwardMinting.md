# 🎟️ **VIP Ticket dApp (Forwarding Minting Policy)**

>Source file: `src/Contracts/Samples/ForwardMinting.hs`

The use case for this example smart contract is to facilitate minting NFTs that serve as tickets to an exclusive event (the contract code can also be adapted for similar use cases).

The contract is parameterized by the *`PubKeyHash`* of a particular event host. Each host creates a unique version of the validator script, which will be used to generate a corresponding unique currency symbol for their tickets.

## **Forwarding Minting Policies**
Rather than minting and distributing the tickets directly through an ordinary minting policy, this contract uses a [***forwarding minting policy***](https://plutonomicon.github.io/plutonomicon/forwarding1) (**FMP**) to instead mint assets indirectly when the conditions of its spending validator have been satisfied.

A FMP is a simple minting policy generated from the hash of a spending validator script. It defers (or "forwards") its minting logic to the validator, and contains only a condition that checks whether the validator has run successfully for at least one of the transaction's inputs. Thus a FMP combines the functionality of a spending validator and a minting policy into a single contract consisting of two interdependent scripts. Users interact with the contract by submitting a transaction that both spends a UTxO at the validator address as input (attaching the validator script to the transaction), and mints a native asset using the corresponding FMP (attaching the policy script).

## **Ticket Vouchers**
In this contract, minting a ticket requires the consumer to unlock a "voucher" UTxO containing a minimal quantity of ADA, which the host has locked at their validator script address.

>The ADA contained in the voucher goes to the consumer, making it behave as a kind of inverted fee paid by the host. To compensate for this, the ticket price could be increased to include this amount, which will ultimately be refunded to the consumer.

### **Validation Logic**
To pass validation, a submitted transaction must satisfy two interconnected conditions:
1. *Unlock a voucher UTxO at the validator script address.*
2. *Mint a token (ticket), the name of which is uniquely determined by the voucher being consumed.*

Note that neither condition can be met without satisfying the other:
1. *To unlock a voucher UTxO, the transaction must mint exactly one token with the correct token name.*
2. *To mint a token with the validator's forwarding minting policy, the corresponding UTxO voucher must be consumed as an input.*

A third validation condition requires the consumer to supply a special redeemer code, granting them the right to consume a voucher (*see next secion*).

### **Voucher Datum & Redeemer Code**
For the validator to be aware of its forwarding minting policy that will be used to satisfy its minting constraint, it must be supplied the *`CurrencySymbol`* (i.e. policy ID) of the FMP script. This is provided as a field on the voucher datum. 

The voucher datum also contains the price (in Lovelace) of its corresponding ticket, allowing the creation of different ticket categories (i.e. 🥈Silver and 🥇Gold tier). The validator checks whether the validating transaction produces an outbound UTxO with this quantity directed to the PubKey address of the host by which the contract is parameterized.

A voucher datum also contains the **SHA-256 hash of a redeemer code**, the plaintext counterpart of which must be provided by the consumer in order to consume it. The access codes contained in the voucher datums are hashed to prevent them being deciphered from on-chain data, which would allow users who were not granted a code to acquire tickets. Redeemer codes also provide a lookup mechanism for off-chain (transaction constructing) code: the plaintext code is hashed to reconstruct its datum counterpart, and voucher UTxOs locked at the validator address are then queried for a match. If a matching UTxO is found, it is included as an input to the transaction and the validation logic determines whether the voucher can be consumed.

The intended use case would be to distribute these access codes through some off-chain promotional mechanism, as opposed to coupling the tickets to specific consumers' public addresses. For instance, a musician could include an access code inside a limited edition vinyl record. In this way, the host doesn't need to know the addresses of their attendees, and a user receiving an access code can freely redistribute it to anyone they like (for example, as a gift, or if they're unable to attend the event themselves). Thus, a code recipient does not need to be an attendee, and the rights to a ticket can be conveniently transferred prior to minting without requiring any on-chain transfer of the asset.

>Due to the difficulty of constructing such a complex datum in the consuming transaction (containing information that is unknown to the end-user), in practice the transactions submitting the vouchers to the validator address would utilize [**inline datums**](https://cips.cardano.org/cips/cip32/).

## **Preventing Counterfeiting & Sabotage**
Because any arbitrary UTxO can be locked at the validator address, it is possible for dishonest users to submit counterfeit vouchers (with a datum containing the hash of an access code they create themselves, and a ticket price of zero). These could then be redeemed to mint counterfeit tickets at virtually no cost. Since the identity of valid UTxOs cannot be known until they are locked at the validator address (which presupposes the existence of the complete validator script), the construction of counterfeit vouchers is fundamentally unpreventable via on-chain logic. Thus, exploitation of the contract in this manner must be countered through off-chain mechanisms. The creator would need to maintain a guestlist of whitelisted voucher UTxOs that they themselves have locked at the script address, as well as the token names that will be generated from these official vouchers (whitelisted token names can be easily generated via a Haskell function, called `getTicketName` in the source code). The guestlist would be used to deny entry to the event to anyone presenting a counterfeited NFT.

In practice, the accompanying off-chain code should also be aware of the whitelist, to prevent construction of transactions that consume unauthorized UTxO inputs. Otherwise such UTxOs (containing identical datums to valid vouchers) could be locked at the validator address by a malicious actor and be inadvertently consumed by users possessing legitimate redeemer codes, instead of the legitimate vouchers. Given the implementation of the off-chain whitelist explained above, this would not result in unauthorized access to the event via counterfeiting: it would instead create an opposite scenario where consumers unwittingly pay the host for invalid tickets and are ultimately denied entry. This could be used as a mechanism to sabotage the event and reputation of the host. To prevent this, off-chain endpoint functions should select transaction inputs not solely on the hashed code in their datums, but additionally by their membership in the whitelist.