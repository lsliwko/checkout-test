package com.checkout

import org.scalatest.{FlatSpec, Matchers}

class CalculateTotalServiceTest extends FlatSpec with Matchers {

  "Checkout" should "convert arguments to items correctly" in {
    CalculateTotalService.toItems(Seq()) should be(Right(Seq()))  //empty/zero test

    CalculateTotalService.toItems(Seq("Soup")) should be(Right(Seq(Soup)))
    CalculateTotalService.toItems(Seq("Bread")) should be(Right(Seq(Bread)))
    CalculateTotalService.toItems(Seq("Milk")) should be(Right(Seq(Milk)))
    CalculateTotalService.toItems(Seq("Apples")) should be(Right(Seq(Apples)))

    CalculateTotalService.toItems(Seq("Soup","Soup","Apples","Soup")) should be(Right(Seq(Soup,Soup,Apples,Soup)))
  }

  it should "return error on undefined item" in {
    CalculateTotalService.toItems(Seq("")) should be(Left("(empty) is undefined"))
    CalculateTotalService.toItems(Seq("Some")) should be(Left("Some is undefined"))
    CalculateTotalService.toItems(Seq("Bread","Some")) should be(Left("Some is undefined"))
  }

  it should "count items correctly" in {
    CalculateTotalService.countItems(Seq()) should be(Map())  //empty/zero test

    CalculateTotalService.countItems(Seq(Soup,Soup,Apples,Soup)) should be(Map(Soup -> 3, Apples -> 1))
  }

  it should "apply Apples 10% Discount Offer correctly" in {
    CalculateTotalService.applyApples10DiscountOffer(Map()) should be(Map(),None)  //empty/zero test

    CalculateTotalService.applyApples10DiscountOffer(Map(Soup -> 1)) should be((Map(Soup -> 1),None))
    CalculateTotalService.applyApples10DiscountOffer(Map(Apples -> 0)) should be((Map(Apples -> 0),None))

    //offer test
    CalculateTotalService.applyApples10DiscountOffer(Map((Soup -> 3),(Apples->7))) should be(
      Map(Soup->3),
      Some("Apples 10% off", (Apples.price * 0.1d * 7).setScale(2))
    )
  }

  it should "apply Bread (with two Soups) 50% Discount Offer correctly" in {
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map()) should be(Map(),None)  //empty/zero test

    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Soup -> 1)) should be(Map(Soup -> 1),None)
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Soup -> 1, Bread -> 3)) should be(Map(Soup -> 1, Bread -> 3),None)
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Soup -> 2)) should be(Map(Soup -> 2),None)
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Bread -> 1)) should be(Map(Bread -> 1),None)
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Soup -> 2, Apples -> 1)) should be(Map(Soup -> 2, Apples -> 1),None)

    //offer test
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Soup -> 2,Bread -> 1)) should be(
      Map(),  //two soups and one breat should be consumed by this offer
      Some("Bread (with two Soups) 50% off", (Bread.price * 0.5).setScale(2))
    )

    //offer multiples test
    CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer(Map(Soup -> 5,Bread -> 2)) should be(
      Map(Soup -> 1), //four soups and two breads should be consumed by this offer
      Some("Bread (with two Soups) 50% off", (Bread.price * 0.5 * 2).setScale(2))
    )
  }

  it should "mix offers correctly" in {
    CalculateTotalService.applyOffers(Map(Soup -> 2, Bread -> 1)) should be(Seq()) //zero/empty test

    CalculateTotalService.applyOffers(
      Map(Soup -> 5,Bread -> 2),
      CalculateTotalService.applyApples10DiscountOffer,
      CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer
    ) should be (
      Seq(("Bread (with two Soups) 50% off", (Bread.price * 0.5 * 2).setScale(2)))
    )

    CalculateTotalService.applyOffers(
      Map(Soup -> 5,Bread -> 2, Apples -> 5),
      CalculateTotalService.applyApples10DiscountOffer,
      CalculateTotalService.applyBreadWithTwoSoups50DiscountOffer
    ) should be (
      Seq(
        ("Apples 10% off", (Apples.price * 0.1d * 5).setScale(2)),
        ("Bread (with two Soups) 50% off", (Bread.price * 0.5 * 2).setScale(2))
      )
    )
  }

  it should "price items correctly" in {
    CalculateTotalService.priceItems(Map()) should be(0d)  //zero/empty test

    CalculateTotalService.priceItems(Map(Soup -> 1)) should be(Soup.price)
    CalculateTotalService.priceItems(Map(Bread -> 1)) should be(Bread.price)
    CalculateTotalService.priceItems(Map(Milk -> 1)) should be(Milk.price)
    CalculateTotalService.priceItems(Map(Apples -> 1)) should be(Apples.price)

    CalculateTotalService.priceItems(Map(Apples -> 2, Soup -> 3)) should be(Apples.price * 2 + Soup.price * 3)
  }

  it should "price offers correctly" in {
    CalculateTotalService.priceOffers(Seq()) should be(0d)  //zero/empty test

    CalculateTotalService.priceOffers(Seq(
      ("Offer A",10d),
      ("Offer B",0.49d)
    )) should be(10.49d)  //zero/empty test
  }

  it should "print amount correctly" in {
    CalculateTotalService.printAmount(0d) should be("£0.00")
    CalculateTotalService.printAmount(-0.1d) should be("-10p")
    CalculateTotalService.printAmount(0.5d) should be("50p")
    CalculateTotalService.printAmount(0.99d) should be("99p")
    CalculateTotalService.printAmount(-0.99d) should be("-99p")
    CalculateTotalService.printAmount(1) should be("£1.00")
    CalculateTotalService.printAmount(-1) should be("-£1.00")
    CalculateTotalService.printAmount(1.3d) should be("£1.30")
    CalculateTotalService.printAmount(-1.3d) should be("-£1.30")
  }

  it should "print receipt correctly" in {
    CalculateTotalService.printReceipt(0, 0, Seq()) should be(  //zero/empty test
      "Subtotal: £0.00\n" +
      "(No offers available)\n" +
      "Total: £0.00\n"
    )

    CalculateTotalService.printReceipt(3, 1, Seq()) should be(
      "Subtotal: £3.00\n" +
      "(No offers available)\n" +
      "Total: £1.00\n"
    )

    CalculateTotalService.printReceipt(3, 1.5, Seq(
      ("Offer A",10d),
      ("Offer B",0.49d)
    )) should be(
      "Subtotal: £3.00\n" +
      "Offer A: -£10.00\n" +
      "Offer B: -49p\n" +
      "Total: £1.50\n"
    )
  }

  it should "correctly price item from arguments (integration test)" in {
    CalculateTotalService.calculate(Seq()) should be(Right( //zero/empty test
      "Subtotal: £0.00\n" +
      "(No offers available)\n" +
      "Total: £0.00\n"
    ))

    CalculateTotalService.calculate(Seq("Bread","Soup","Soup","Apples","Apples")) should be(Right(
      "Subtotal: £4.10\n" +
      "Apples 10% off: -20p\n" +
      "Bread (with two Soups) 50% off: -40p\n" +
      "Total: £3.50\n"
    ))

    CalculateTotalService.calculate(Seq("Bread","Soup","Soup","Apples","Some")) should be(Left("Some is undefined"))
  }
}