package com.checkout


sealed abstract class Item(val price : Double)

case class Apple() extends Item(0.6)
case class Orange() extends Item(0.25)
