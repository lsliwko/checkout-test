package com.checkout


object CalculateTotalService {

  def calculateTotal(items : Iterable[Item]) : Double = {
    //gives Map(Orange -> 2, Apple -> 3)
    val itemsCount  = items.groupBy(identity).mapValues(_.size)

    itemsCount.map{case (item, count) => calulateTotalPerItemType(item, count)}.sum
  }

  private def calulateTotalPerItemType(item : Item, count : Int) : Double = {
    item match {
      case Apple() => ((count + 1)/2) * Apple().price       //buy one, get one free on Apples
      case Orange() => ((count + 1)*2/3) * Orange().price   //3 for the price of 2
    }
  }

  def parseItems(args : Iterable[String]) : Iterable[Item] = {
    args.map{
      case "apple" => Apple()
      case "orange" => Orange()
      case _ => throw new IllegalArgumentException
    }
  }

  def main(args : Array[String]): Unit = {
    val items = parseItems(args)

    println("Total=" + calculateTotal(items))
  }
}
