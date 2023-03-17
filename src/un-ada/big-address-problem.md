# A solution to the Big-Address staking problem

Many DeFi apps on Cardano keep all of the funds they control at a single address. The funds are locked not just by the same Plutus script but allocated to the same staking credential. Whoever controls the staking credential can claim the rewards of the staked Ada and, by delegating to a stake pool of their choice, has considerable power within the consensus mechanism that is securing the entire Cardano network. If all of an app's staking rewards are controlled by a single entity, then how decentralised can the app really claim to be?

Another issue arises if the amount of Ada at the app's address exceeds the maximum amount that can be allocated to a single stake pool. In that case the excess Ada will not count towards staking rewards and some rewards will be forfeited. This makes it less attractive for liquidity providers to send their Ada to popular applications.

In this blog post we propose a novel, algorithmic, decentralised Ada stablecoin that solves the "Big-Address staking problem" and allows for fine-grained delegation of large Ada reserves.

## Examples

The Djed stablecoin[reference][^1], recently launched on mainnet, has a reserve of about 31 Million Ada, all locked in a single UTxO and staked to a single address. This is almost half of 71 Million Ada, the threshold of stake pool saturation. MinSwap, another DeFi project, is much closer to hitting the maximum amount, with 60.5 Million Ada [locked](https://cardanoscan.io/address/addr1z8snz7c4974vzdpxu65ruphl3zjdvtxw8strf2c2tmqnxz2j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq0xmsha) by its Plutus script.

Below is a list of the top ten Cardano apps by locked Ada value, as of March 17 2023. Source: [https://dappsoncardano.com/](https://dappsoncardano.com/)

![Top ten apps by locked Ada value](doc/2023-03-17-top-ten-apps.png)

## Potential Solutions

Of course an easy solution to the problem of locking too much Ada under one stake key would be to split up the app's state across multiple UTxOs with different stake credentials while keeping the same payment credential. This is hard to do for several reasons. First, the user experience would suffer. We are used to associating "one app" with "one address" and many tools such as CardanoScan emphasize this pattern by providing data on a per-address basis. Using different stake keys for a single app would also require some logic to decide which stake key should be used for each transaction that interacts with the app. In a fully decentralised app this logic would need to be encoded in the Plutus scripts themselves, adding a tax in form of higher script execution cost to every transaction. More logic also means a bigger attack surface, an increased maintenance burden and a higher cost for code audits. Finally, some apps require all their state to be kept in a single UTxO, so they cannot use more than one address ever. Djed in its current iteration is one example.

Another solution would be to use a Plutus script as the staking credential and add a governance structure (like a DAO) that decides which pool to allocate the funds to, and how to distribute the rewards. This approach is problematic because the governance structure is not just another feature, but an entirely new app in itself. The complexity of the DAO can easily eclipse that of the original app whose staking rewards the DAO is supposed to manage. And the 71 Million Ada limit for the amount that can be delegated to a single pool still applies.

Staking centralisation a problem for many members of the Cardano community, not just for users of DeFi apps, and there are proposals for fixing it on other layers of the system. [Conclave](https://iohk.io/en/research/library/papers/conclave-a-collective-stake-pool-protocol/) addresses the issue indirectly, by enabling stake pools to have more than a single operator. Implementing conclave as described in the paper would require major changes to the ledger, so it is not a practical solution in the short term.
(TBD: Multi Delegation?)

## An Ada stablecoin for flexible delegation

To solve the Big-Address staking problem we introduce `unAda`, a new Ada stablecoin. This is a token that can always be swapped for Lovelace on a fixed exchange rate of 1 to 1. This property is guaranteed by the minting policy of `unAda`, which ensures that for every single `unAda` there is one "real" Ada deposited in the reserves of the stablecoin. As the exchange rate is fixed there is no risk of de-pegging, and no need for mechanisms aimed at reducing market manipulation or trading against the bank. `unAda` is permissionless (every user of Cardano can create and burn any amount of it) and decentralised both in terms of the off-chain infrastructure required to operate it (none) and the number of on-chain UTxOs that hold the reserves (unlimited).

How does the Ada stablecoin solve the staking problem? `unAda` always has the same nominal value as real Ada, so it can be used in all places where real Ada is used at the moment. For example, one can set up a liquidity pool for trading `unAda` with any other Cardano token on a decentralised exchange (DEX) such as Minswap or Muesliswap. Or start up a new instance of the Djed USD stablecoin that is backed by `unAda` instead of Ada.

Using `unAda` instead of Ada has two effects.

First, the amount of real Ada at the Big-Address shrinks to almost nothing (in practice, it must be at least the minimum amount required for each UTxO - about 2 Ada). The economic properties of the contract that underlies the Big-Address don't change for users of the app, because `unAda` can always be redeemed in full for real Ada.

Second, the real Ada whose `unAda` equivalent is deposited at the Big-Address can now be staked freely to any number of stake pools, simply by creating many different `unAda` UTxOs. Each user of `unAda` can choose their own staking certificate at the time of creating the currency. No additional protocol is required for changing the staking certificate, withdrawal of staking rewards, etc. A user can even split their Ada across different stake pools and still put the entire amount towards the same Big-Address app.

One particular benefit of `unAda` is that it requires no permission from the operators of a DEX, and no changes to the implementation of the DEX. `unAda` and Ada can happily coexist as `unAda` is just another Cardano token.

## Potential issues and their mitigation

*Manipulating the staking credential* A malicious user could attempt to change the staking credential of a portion of `unAda` by withdrawing a specific amount of `unAda` from the Big-Address and exchanging it for real Ada, thereby spending the UTxO with the delegation. To address this, the UTxOs that hold the reserves have a time lock, keeping the Ada in place for at least one epoch to ensure that they count towards the stake of the intended stake pool. This does not completely prevent mainpulation of the staking credential, but it does raise the barrier for the attack, due to the waiting time involved when targeting specific staking credentials.

## Implementation Details

The solution involves two Plutus scripts: A minting policy and a validator. The minting policy controls the minting and burning of `unAda`. It ensures that for every `unAda` minted, one real Ada is locked by the validator. When burning `unAda` the minting policy ensures that the same amount of Ada is released, and the rest remains locked up by the validator. The validator script implements the time lock and ensures that the invariant of the minting policy holds (ie. that the difference in Ada locked is equal to the amount of `unAda` minted).

## The price of `unAda`

One would expect the price of `unAda` to be exactly one Ada, because they are interchangeable. But in reality the price of `unAda` is likely going to be slightly less than one Ada, for two reasons. First, `unAda` does not earn any staking rewards while real Ada does. So one `unAda` will always be one `unAda`, as the saying goes, but one real Ada today will actually be worth approx. 1.06 Ada a year from now, because Ada always includes a claim to future staking rewards. Because of that, the future value of the Ada that backs `unAda` needs to be discounted in order to arrive at a fair value for `unAda`. Second, while it is guaranteed that one `unAda` can always be converted to one Ada, there is no guarantee that the conversion can take place *right now*: The transaction could be rejected due to UTxO contention (someone else spent the UTxO and we need to select a different one), or there could be a waiting period if all reserve UTxOs are still time locked. So `unAda` is less liquid than real Ada.
