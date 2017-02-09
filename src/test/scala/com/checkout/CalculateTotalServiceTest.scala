package com.checkout

import org.scalatest.{FlatSpec, Matchers}

class CalculateTotalServiceTest extends FlatSpec with Matchers {

  "Checkout" should "parse known items correctly" in {
    CalculateTotalService.parseItems(Iterable("orange")) should be(Iterable(Orange()))

    CalculateTotalService.parseItems(Iterable("apple")) should be(Iterable(Apple()))

    CalculateTotalService.parseItems(Iterable("orange", "orange")) should be(Iterable(Orange(),Orange()))

    CalculateTotalService.parseItems(Iterable("orange", "apple")) should be(Iterable(Orange(),Apple()))

  }

  it should "parse empty items list as empty" in {
    CalculateTotalService.parseItems(Iterable()) should be(Iterable())
  }

  it should "throw error when parsing unknown item" in {

    a[IllegalArgumentException] should be thrownBy {
      CalculateTotalService.parseItems(Iterable("dummy"))
    }

    a[IllegalArgumentException] should be thrownBy {
      CalculateTotalService.parseItems(Iterable("apple","dummy"))
    }
  }

  it should "calculate items prices correctly" in {
    CalculateTotalService.calculateTotal(Iterable(Apple())) should be(0.60)

    CalculateTotalService.calculateTotal(Iterable(Orange())) should be(0.25)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Apple())) should be(0.85)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Orange(), Orange(), Apple(), Apple())) should be(1.10)
  }

  it should "parse empty empty items list as zero" in {
    CalculateTotalService.calculateTotal(Iterable()) should be(0)
  }

  it should "calculate discounted Apples price correctly" in {
    CalculateTotalService.calculateTotal(Iterable(Apple())) should be(0.60)

    CalculateTotalService.calculateTotal(Iterable(Apple(), Apple())) should be(0.60)

    CalculateTotalService.calculateTotal(Iterable(Apple(), Apple(), Apple())) should be(1.20)

    CalculateTotalService.calculateTotal(Iterable(Apple(), Apple(), Apple(), Apple())) should be(1.20)
  }

  it should "calculate discounted Oranges price correctly" in {
    CalculateTotalService.calculateTotal(Iterable(Orange())) should be(0.25)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Orange())) should be(0.50)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Orange(), Orange())) should be(0.50)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Orange(), Orange(), Orange())) should be(0.75)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Orange(), Orange(), Orange(), Orange())) should be(1.00)

    CalculateTotalService.calculateTotal(Iterable(Orange(), Orange(), Orange(), Orange(), Orange(), Orange())) should be(1.00)
  }
}
