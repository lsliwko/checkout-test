package com.checkout

import java.text.NumberFormat
import java.util.Locale

import cats.implicits._

sealed abstract class Item(val name : String) {
  def price : BigDecimal  //can be used to retrieve price dynamically
}

object Item{
  def unapply(arg: String): Option[Item] = arg match {
    case Soup.name => Some(Soup)
    case Bread.name => Some(Bread)
    case Milk.name => Some(Milk)
    case Apples.name => Some(Apples)
    case _ => Option.empty[Item]
  }
}

final case object Soup extends Item("Soup") { def price = BigDecimal.valueOf(0.65) }
final case object Bread extends Item("Bread") { def price = BigDecimal.valueOf(0.80) }
final case object Milk extends Item("Milk") { def price = BigDecimal.valueOf(1.30) }
final case object Apples extends Item("Apples") { def price = BigDecimal.valueOf(1.00) }

object CalculateTotalService {

  type ErrorType = String
  type ItemsBasketType = Map[Item, Int] //item -> amount
  type OfferType = (String, BigDecimal) //(name, discount amount)
  type OfferFunctionType = ItemsBasketType => (ItemsBasketType, Option[OfferType]) //(items basket after offer, applied offer)

  private val currencyFormatter = NumberFormat.getCurrencyInstance(Locale.UK)

  /**
    * Converts a sequence of String to Sequence of Items
    * Note: returns Left if any of items cannot be converted
    * @param args
    * @return
    */
  def toItems(args : Seq[String]) : Either[ErrorType, Seq[Item]] =
    args.map { arg => arg match {
      case Item(item) => item //unapply from Item is used to match
      case arg@_ if arg=="" => s"(empty) is undefined": ErrorType
      case arg@_ => s"${arg} is undefined": ErrorType
    }
    }.foldLeft(Seq.empty[Item].asRight[ErrorType]) {
      case (Right(items), item: Item) => Right(items :+ item)
      case (_, exception: ErrorType) => exception.asLeft
      case (left, _) => left
    }

  /**
    * Counts items and returns items basket
    * @param items
    * @return
    */
  def countItems(items : Seq[Item]) : ItemsBasketType =
    items.groupBy(item => item).map{pair => (pair._1, pair._2.size)}

  /**
    * Applies all argument offers and returns result items basket
    * Note: offers will be run in specified order
    * @param itemsBasket input items
    * @param offers input offers
    * @return (result items basket, applied offers)
    */
  def applyOffers(
                   itemsBasket : ItemsBasketType,
                   offers : OfferFunctionType*
                 ) : Seq[OfferType] =
    offers.foldLeft((itemsBasket, Seq.empty[OfferType])) { case ((itemsBasket, appliedOffers), offer) =>
      val (itemsBasketAfterOffer, appliedOfferOption) = offer.apply(itemsBasket)  //apply offer
      (itemsBasketAfterOffer, appliedOffers ++ appliedOfferOption)  //carry over items basket and append applied offer (if used)
    }._2  //return only applied offers

  /**
    * Totals all items' prices
    * @param itemsBasket
    * @return
    */
  def priceItems(itemsBasket: ItemsBasketType) : BigDecimal =
    itemsBasket.foldLeft(BigDecimal.valueOf(0)) { case (total, (item, amount)) => total + item.price * amount}
      .setScale(currencyFormatter.getMinimumFractionDigits, BigDecimal.RoundingMode.HALF_UP)

  /**
    * Totals all applied offers' discounts
    * @param appliedOffers
    * @return
    */
  def priceOffers(appliedOffers : Seq[OfferType]) : BigDecimal =
    appliedOffers.foldLeft(BigDecimal.valueOf(0)) { case (total, (_, discount)) => total + discount}
      .setScale(currencyFormatter.getMinimumFractionDigits, BigDecimal.RoundingMode.HALF_UP)

  /**
    * Formats amount to £ and p
    * @param amount
    * @return
    */
  def printAmount(amount : BigDecimal) : String =
    if (
      (amount.compare(BigDecimal.valueOf(0))!=0) && //for 0, it should return "£0.00"
      (amount.abs.compare(1)<0) //for -0.99 to 0.99 it should return XXp
    ) s"${(amount*100).setScale(0)}p"
    else currencyFormatter.format(amount)

  /**
    * Prints receipt
    * @param subtotal
    * @param total
    * @param appliedOffers
    * @return
    */
  def printReceipt(subtotal : BigDecimal, total : BigDecimal, appliedOffers : Seq[OfferType]) : String =
    s"Subtotal: ${printAmount(subtotal)}\n" +
      {
        if (appliedOffers.isEmpty) "(No offers available)\n"
        else appliedOffers.map { case (name, amount) => s"${name}: ${printAmount(-amount)}\n"}.mkString
      } +
      s"Total: ${printAmount(total)}\n"

  /**
    * Calculates and prints receipt
    * @param args
    * @return
    */
  def calculate(args : Seq[String]) : Either[String,String] =
    for {
      items <- toItems(args)
      itemsBasket = countItems(items)
      appliedOffers = applyOffers(itemsBasket, applyApples10DiscountOffer, applyBreadWithTwoSoups50DiscountOffer)
      subtotal = priceItems(itemsBasket)
      discount = priceOffers(appliedOffers)
      total = subtotal - discount
    } yield printReceipt(subtotal, total, appliedOffers)


  // ---- OFFERS ----
  //Offers return (Items Basket After Offer, (Option[Offer's name, Offer Discount])

  //Apples have a 10% discount off their normal price this week
  def applyApples10DiscountOffer : OfferFunctionType = itemsBasket => {
    itemsBasket.get(Apples).fold((itemsBasket, Option.empty[OfferType])) { amount =>
      if (amount==0) (itemsBasket, None)  //in case his function will be called directly
      else (
        itemsBasket-Apples,    //remove apples from result basket
        Some(
          "Apples 10% off",
          BigDecimal.valueOf(amount * 0.1d).setScale(currencyFormatter.getMinimumFractionDigits, BigDecimal.RoundingMode.HALF_UP)
        ) //calculate discount for this offer
      )
    }
  }

  //Buy 2 tins of soup and get a loaf of bread for half price
  def applyBreadWithTwoSoups50DiscountOffer : OfferFunctionType = itemsBasket => {
    (itemsBasket.get(Soup), itemsBasket.get(Bread)) match {
      case (Some(amountOfSoups), Some(amountOfBreads)) if (amountOfSoups>1) && (amountOfBreads>0) =>  //in case his function will be called directly
        val offerMultiplier = Math.min(amountOfSoups/2,amountOfBreads) //calculate multiplier of offer
        (
          (itemsBasket |+| Map(Soup -> -2*offerMultiplier) |+| Map(Bread -> -offerMultiplier)).filterNot(_._2==0), //remove discounted soups and bread
          Some(
            "Bread (with two Soups) 50% off",
            (offerMultiplier * Bread.price * 0.5d).setScale(currencyFormatter.getMinimumFractionDigits, BigDecimal.RoundingMode.HALF_UP)
          ) //calculate discount for this offer
        )
      case _ => (itemsBasket, None)
    }
  }

  // -----

  def main(args : Array[String]): Unit = {

    val result = calculate(args)

    //result is type of Either
    result match {
      case Right(receipt) =>
        println(receipt)
      case Left(error) =>
        println("ERROR:")
        println(error)
    }
  }

}