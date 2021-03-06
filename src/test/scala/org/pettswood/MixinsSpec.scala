package org.pettswood

import org.specs2.mutable._
import org.specs2.mock._
import org.mockito.Matchers._
import org.pettswood.stubs._
import org.pettswood.stubs.ExpectedConcept

class MixinsSpec extends SpecificationWithJUnit with Mockito {

  class Fixture {
    val domain = mock[DomainBridge]
    val mixin = new Mixins(domain)
  }

  "Mixins" should {
    "teach the domain about individual concepts" in {
      val fixture = new Fixture()

      fixture.mixin.cell("org.pettswood.stubs.ExpectedConcept")

      there was one(fixture.domain).learn(same("org.pettswood.stubs.ExpectedConcept"), any[ExpectedConcept])
    }

    "allow multiple concepts in multiple cells" in {
      val fixture = new Fixture()

      fixture.mixin.cell("org.pettswood.stubs.ExpectedConcept")
      fixture.mixin.cell("org.pettswood.stubs.AnotherExpectedConcept")

      there was one(fixture.domain).learn(same("org.pettswood.stubs.ExpectedConcept"), any[ExpectedConcept])
      there was one(fixture.domain).learn(same("org.pettswood.stubs.AnotherExpectedConcept"), any[AnotherExpectedConcept])
    }

    "allow addition of multiple concepts" in {
      val fixture = new Fixture()

      fixture.mixin.cell("org.pettswood.stubs.SomeMixin")

      there was one(fixture.domain).learn(same("ExpectedConcept"), any[ExpectedConcept])
      there was one(fixture.domain).learn(same("AnotherExpectedConcept"), any[AnotherExpectedConcept])
    }

    "allow use of multiple Mixins anywhere in the test" in {
      val domain = mock[DomainBridge]

      new Mixins(domain).cell("org.pettswood.stubs.SomeMixin")
      new Mixins(domain).cell("org.pettswood.stubs.YetAnotherExpectedConcept")

      there was one(domain).learn(same("ExpectedConcept"), any[ExpectedConcept])
      there was one(domain).learn(same("AnotherExpectedConcept"), any[AnotherExpectedConcept])
      there was one(domain).learn(same("org.pettswood.stubs.YetAnotherExpectedConcept"), any[YetAnotherExpectedConcept])
    }

    "Supports concepts defined in PettswoodConfig" in {
      val fixture = new Fixture()
      PettswoodConfig.mixinPackages ++= Seq("org.pettswood.stubs")

      fixture.mixin.cell("ExpectedConcept")

      there was one(fixture.domain).learn(same("ExpectedConcept"), any[ExpectedConcept])
    }

    "Throws underlying exception when Mixins fail to be instantiated properly" in {
      val fixture = new Fixture()

      fixture.mixin.cell("org.pettswood.BadMixin") must throwAn[IllegalArgumentException]
    }
  }
}

class BadMixin(domainBridge: DomainBridge) extends Mixin(domainBridge) {
  throw new IllegalArgumentException("Religion")
}