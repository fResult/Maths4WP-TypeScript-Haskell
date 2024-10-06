type SKU = String
type Id = Integer

data Product = Product
  { sku :: SKU
  , quantity :: Int
  }
  deriving (Show)

data Cart = Cart
  { cartId :: Id
  , cartItems :: [Product]
  }
  deriving (Show)

data Order = Order
  { orderId :: Id
  , orderItems :: [Product]
  }
  deriving (Show)

newtype PendingOrder = Pending Order deriving (Show)
newtype PaidOrder = Paid Order deriving (Show)
newtype PreparedOrder = Prepared Order deriving (Show)
newtype ShippedOrder = Shipped Order deriving (Show)

add :: Cart -> Product -> Cart
add (Cart id products) product = Cart id (product : products)

checkout :: Cart -> PendingOrder
checkout (Cart id products) = Pending (Order id products)

pay :: PendingOrder -> PaidOrder
pay (Pending order) = Paid order

prepare :: PaidOrder -> PreparedOrder
prepare (Paid order) = Prepared order

ship :: PreparedOrder -> ShippedOrder
ship (Prepared order) = Shipped order

{-- MkCart Functor --}
mkCart :: [Product] -> Cart
mkCart = Cart 1

-- Test data
p1 :: Product
p1 = Product "sku1" 1
p2 :: Product
p2 = Product "sku2" 2
p3 :: Product
p3 = Product "sku3" 3
products :: [Product]
products = [p1, p2, p3]

cart :: Cart
cart = mkCart products

p4 :: Product
p4 = Product "sku4" 4
cartAdded1 :: Cart
cartAdded1 = add cart p4

p5 :: Product
p5 = Product "sku5" 5
cartAdded2 :: Cart
cartAdded2 = add cartAdded1 p5

pendingOrder :: PendingOrder
pendingOrder = checkout cartAdded2
paidOrder :: PaidOrder
paidOrder = pay pendingOrder
preparedOrder :: PreparedOrder
preparedOrder = prepare paidOrder
shippedOrder :: ShippedOrder
shippedOrder = ship preparedOrder
