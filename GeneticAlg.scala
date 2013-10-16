package com.czechscala.blank

import scala.util.Random
import scala.annotation.tailrec

trait FitnessLike extends (ChromosomeLike => Double)
trait MutationLike extends (ChromosomeLike => ChromosomeLike)
trait CrossoverLike extends ((ChromosomeLike, ChromosomeLike) => (ChromosomeLike, ChromosomeLike))

trait ChromosomeLike {
  val genes: Seq[Int]
  def value: Int
}

trait PopulationLike {
  val chromosomes: Seq[ChromosomeLike]
  def best(fitness: FitnessLike): ChromosomeLike
  def nextPopulation(crossover: CrossoverLike, mutation: MutationLike, fitness: FitnessLike): Population
}

class Chromosome(val genes: Seq[Int]) extends ChromosomeLike {
  override def value =
    genes.zipWithIndex.foldLeft(0) {
      (result, valueAndIdx) => result + valueAndIdx._1 * Math.pow(2, valueAndIdx._2).toInt
    }
}

class Population(val chromosomes: Seq[ChromosomeLike]) extends PopulationLike {

  override def best(fitness: FitnessLike) =
    chromosomes.reduceLeft((best, candidate) =>
      if (fitness(candidate) > fitness(best)) candidate
      else best)

  override def nextPopulation(crossover: CrossoverLike, mutation: MutationLike, fitness: FitnessLike) = {
    @tailrec
    def selection(sum: Int, ch: Seq[ChromosomeLike], r: Int): ChromosomeLike = {
      val s = sum + fitness(ch.head).toInt
      if (s > r) ch.head
      else selection(s, ch.tail, r)
    }
    val S = chromosomes.foldLeft(0)((a, b) => a + fitness(b).toInt)
    new Population(
      for (i <- 1 to chromosomes.length) yield mutation(selection(0, chromosomes, Random.nextInt(S))))
  }
}

class Fitness extends FitnessLike {
  override def apply(chromosome: ChromosomeLike) = {
    val x = chromosome.value
    Math.pow(x, 2)
  }
}

// cross two chromosomes on random index (assumed chromosomes are same length)
class Crossover extends CrossoverLike {
  def apply(x: ChromosomeLike, y: ChromosomeLike) = {
    val crossPoint = Random.nextInt(x.genes.length);
    def cross(x: ChromosomeLike, y: ChromosomeLike) = {
      new Chromosome(
        for (i <- 0 until x.genes.length)
          yield if (i < crossPoint) x.genes(i) else y.genes(i))
    }
    (cross(x, y), cross(y, x))
  }
}

// Switch one random gene in chromosome from 1 to 0 or vice versa
class Mutation extends MutationLike {
  override def apply(x: ChromosomeLike) = {
    val mutateIdx = Random.nextInt(x.genes.length)
    new Chromosome(
      for (i <- 0 until x.genes.length)
        yield if (i == mutateIdx) 1 - x.genes(i) else x.genes(i))
  }
}

/* MAIN */
// http://www.obitko.com/tutorials/genetic-algorithms/index.php
object Main {
  def main(args: Array[String]) {
    var population = new Population(
      for (i <- 1 to 10) yield new Chromosome( // number of chromosomes in population
        for (j <- 1 to 8) yield Random.nextInt(2))) // number of genes in chromosome

    for (i <- 1 to 100) {
      population = population.nextPopulation(new Crossover, new Mutation, new Fitness)
    }
    println(population.best(new Fitness).value)
  }
}
