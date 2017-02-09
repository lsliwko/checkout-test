package com.checkout


object CalculateTotalService {

  def calculateTotal(items : Iterable[Item]) : Double = {
    items.foldLeft(0d){ _ + _.price }
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
