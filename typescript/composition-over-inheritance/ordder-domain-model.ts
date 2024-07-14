type Maybe<T> = T | null

class DomainOrder {}

// pending, queueing
class ActiveOrder extends DomainOrder {
    constructor(private _id: number, private _fills: Record<string, any>[]) {
        super()
    }

    get id(): number {
        return this._id
    }

    get fills(): Record<string, any> {
        return this._fills
    }
}

interface Notifiable {
    symbol: string
    price: number
    quantity: number
    status: OrderStatus
}

interface Finishable {
    fills: Record<string, any>[]
}

// PartiallyFilled, Filled
class FilledOrder implements Notifiable {
    constructor(
        private _id: number,
        private _fills: Record<string, any>[],
        private _symbol: string,
        private _price: number,
        private _quantity: number,
        private _status: OrderStatus
    ) {}

    get id(): number {
        return this._id
    }

    get fills(): Record<string, any>[] {
        return this._fills
    }

    get symbol(): string {
        return this._symbol
    }

    get price(): number {
        return this._price
    }

    get quantity(): number {
        return this._quantity
    }

    get status(): OrderStatus {
        return this._status
    }

    //get cancelled quantity
    //get average filled price
}
class LimitOrder extends DomainOrder {
    constructor(private _id: number) {
        super()
    }

    get id(): number {
        return this._id
    }
}
class MarketOrder {
    constructor(private _id: number) {}

    get id(): number {
        return this._id
    }
}
// class NotifiableOrder {
//     constructor(
//         private _id: number,
//         private _fills: Record<string, any>,
//         private _status: OrderStatus
//     ) {
//         if (
//             _status != OrderStatus.Rejected ||
//             _status != OrderStatus.Filled ||
//             _status != OrderStatus.PartiallyFilled
//         ) {
//             throw new XPathExpression()
//         }
//     }

//     get id(): number {
//         return this._id
//     }

//     get fills(): Record<string, any> {
//         return this._fills
//     }

//     get status(): OrderStatus {
//         return this._status
//     }
// }

enum OrderStatus {
    Pending,
    Queueing,
    PartiallyFilled,
    Filled, // Completed
    Canceled,
    Rejected
}

enum OrderType {
    Limit,
    Market
}

// Data Class (DB Model)
class Order {
    constructor(
        private _id: number,
        private _status: OrderStatus,
        private _type: OrderType,
        private _fills?: Record<string, any>[]
    ) {}

    get id(): number {
        return this._id
    }

    get status(): OrderStatus {
        return this._status
    }

    get type(): OrderType {
        return this._type
    }

    get fills(): Maybe<Record<string, any>[]> {
        return this._fills || null
    }
}

class XOrder<T extends DomainOrder> {
    // constructor for all kind of Orders
    constructor(private _order: T) {}

    // getFilledQuantity :: (Finishable) -> number // for ActiveOrder and FilledOrder
    getFilledQuantity(order: Finishable): number {
        return order.fills.reduce((acc, fill) => acc + fill.quantity, 0)
    }
    //get
}

console.log(new Order(1, OrderStatus.Pending, OrderType.Limit, []))

class NotificationService {
    notify(order: Notifiable): void {
        const { price, quantity, status, symbol } = order

        console.log('Send notification to customer!!')
        console.log(`Symbol: ${symbol}`)
        console.log(`Price: ${price}`)
        console.log(`Quantity: ${quantity}`)
        console.log(`Status: ${status}`)
    }
}
