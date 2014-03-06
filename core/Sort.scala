package com.qiuzhuang.core


def sort(xs: Array[Int]): Array[Int] = {
	if (xs.length <= 1) xs
	else {
		val pivot = xs(xs.length/2)
		Array.concat(
			sort(xs filter (x => pivot > x)),
			     xs filter (pivot ==),
			sort(xs filter (pivot <))
		)
	}
}

def while (p: => Boolean) (s: => Unit) {
	if (p) {s; While(p)(s)}
}

class Auction(seller: Actor, minBid: Int, closing: Date) extends Actor {
	val timeToShutdown = 3600000000
	val bidIncrement = 10

	def act() {
		var maxBid = minBid - bidIncrement
		var maxBidder: Actor = null
		var running = true

		while (running) {
			receiveWithin((closing.getTime() - new Date().getTime())) {
				case Offer(bid, client) =>
				  if (bid >= maxBid + bidIncrement) {
				  	if (maxBid >= minBid) maxBidder ! BeatenOffer(bid)
				  	maxBid = bid; maxBidder = client; client ! BestOffer
				  } else {
				  	client ! BeatenOffer(maxBid)
				  }
				case Inquire(client) =>
				  client !Status(maxBid, closing)
				case TIMEOUT =>
				  if (maxBid >= minBid) {
				  	val reply = AcutionConcluded(seller, maxBidder)
				  	maxBidder ! reply; seller ! reply
				  } else {
				  	seller ! AcutionFailed
				  }
				  receiveWithin(timeToShutdown) {
				  	case Offer(_, client) => client ! AuctionOver
				  	case TIMEOUT => running = false
				  }
			}
		}
	}
}

def improve(gussess: Double, x: Doulbe) = 
  (gussess + x / gussess) / 2

def isGoodEnough(gussess: Double, x: Double) = 
  abs(gussess * gussess - x) < 0.001

def sqrt(gussess: Double, x: Double) = {
	def improve(gussess: Double, x: Doulbe) = (gussess + x / gussess) / 2

	def isGoodEnough(gussess: Double, x: Double) = abs(gussess * gussess - x) < 0.001

	  if (isGoodEnough(gussess, x)) gussess
  else sqrt(improve(gussess, x), x)

}


