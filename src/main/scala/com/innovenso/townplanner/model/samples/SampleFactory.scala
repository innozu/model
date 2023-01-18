package com.innovenso.townplanner.model.samples

import com.innovenso.townplanner.model.EnterpriseArchitecture
import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.concepts.views.FlowView
import com.innovenso.townplanner.model.language.Element
import com.innovenso.townplanner.model.meta._
import com.thedeanda.lorem.LoremIpsum
import fish.genius.logging.Loggable

import java.security.SecureRandom
import java.util.{Currency, Locale, UUID}

case class SampleFactory(ea: EnterpriseArchitecture) extends Loggable {
  private val random = new SecureRandom()
  private val lorem = LoremIpsum.getInstance()

  def locale: Locale = {
    val index = randomInt(Locale.getAvailableLocales.length)
    Locale.getAvailableLocales.toList(index)
  }

  def randomInt(bound: Int): Int =
    random.nextInt(bound) + 1

  def randomDouble(bound: Double): Double = random.nextDouble(bound)

  def url: String = lorem.getUrl

  def email: String = lorem.getEmail

  def word: String = lorem.getWords(1)

  def description: String = lorem.getWords(5, 100)

  def id: String = UUID.randomUUID().toString.replace("-", "_")

  def title: String = lorem.getTitle(1, 3)

  def name: String = lorem.getNameFemale

  def unitCount: UnitCount = UnitCount(randomDouble(20))

  def unitOfMeasure: UnitOfMeasure = UnitOfMeasure(title)

  def monetaryAmount: MonetaryAmount =
    MonetaryAmount(randomDouble(1000), Currency.getInstance("EUR"))

  def actor: Actor = ea describes Actor() as { it =>
    it has Title(title)
    it has Description(description)
  }

  def person: Person = ea describes Person() as { it =>
    it has Title(title)
    it has Description(description)
  }

  def team: Team = ea describes Team() as { it =>
    it has Title(title)
    it has Description(description)
  }

  def knowledge(
      person: Person,
      technology: Technology,
      level: KnowledgeLevel
  ): Knowledge =
    ea has Knowledge(
      source = person.key,
      target = technology.key,
      level = level
    )

  def teamMember(team: Team): Person = ea describes Person() as { it =>
    it has Title(name)
    it has Description(description)
    it isPartOf team
  }

  def tag: Tag = ea describes Tag() as { it =>
    it has Title(title)
    it has Description(description)
  }

  def verdict: ArchitectureVerdict = randomInt(4) match {
    case 1 => BeInvestedIn(description)
    case 2 => BeEliminated(description)
    case 3 => BeTolerated(description)
    case 4 => BeMigrated(description)
  }

  def language: Language = ea describes Language() as { it =>
    it has Title(title)
    it has Description(description)
    it should verdict
  }

  def framework: Framework = ea describes Framework() as { it =>
    it has Title(title)
    it has Description(description)
    it should verdict
  }

  def tool: Tool = ea describes Tool() as { it =>
    it has Title(title)
    it has Description(description)
    it should verdict
  }

  def technique: Technique = ea describes Technique() as { it =>
    it has Title(title)
    it has Description(description)
    it should verdict
  }

  def platformTechnology: Platform = ea describes Platform() as { it =>
    it has Title(title)
    it has Description(description)
    it should verdict
  }

  def microservice(
      system: ItSystem,
      name: Option[String] = None
  ): Microservice = {
    val implementingLanguage = language
    val implementingFramework = framework
    ea describes Microservice() as { it =>
      it has Title(name.getOrElse(title))
      it has Description(description)
      it isImplementedBy implementingLanguage
      it isImplementedBy implementingFramework
      it isPartOf system
      it has API(style = RestAPI(), scope = PublicScope())
    }
  }

  def database(system: ItSystem, name: Option[String] = None): Database = {
    val implementingLanguage = language
    ea describes Database() as { it =>
      it has Title(name.getOrElse(title))
      it has Description(description)
      it isImplementedBy implementingLanguage
      it isPartOf (system)
    }
  }

  def queue(system: ItSystem, name: Option[String] = None): Queue = {
    val implementingLanguage = language
    ea describes Queue() as { it =>
      it has Title(name.getOrElse(title))
      it has Description(description)
      it isImplementedBy implementingLanguage
      it isPartOf (system)
    }
  }

  def ui(system: ItSystem, name: Option[String] = None): WebUI = {
    val implementingLanguage = language
    val implementingFramework = framework
    ea describes WebUI() as { it =>
      it has Title(name.getOrElse(title))
      it has Description(description)
      it isImplementedBy implementingLanguage
      it isImplementedBy implementingFramework
      it isPartOf system
    }
  }

  def enterprise: Enterprise = ea describes Enterprise() as { it =>
    it has Title(title)
    it has Description(description)
    it has Website(url, title)
    it has Wiki(url, title)
    (1 to randomInt(5)).foreach(_ => it has Strength(description = description))
    (1 to randomInt(5)).foreach(_ => it has Weakness(description = description))
    (1 to randomInt(5)).foreach(_ =>
      it has Opportunity(description = description)
    )
    (1 to randomInt(5)).foreach(_ => it has Threat(description = description))

  }

  def capability(
      servedEnterprise: Option[Enterprise] = None,
      parentCapability: Option[BusinessCapability] = None,
      tags: List[Tag] = Nil
  ): BusinessCapability =
    ea describes BusinessCapability() as { it =>
      it has Title(title)
      it has Description(description)
      if (servedEnterprise.isDefined) it serves servedEnterprise.get
      if (parentCapability.isDefined) it serves parentCapability.get
      tags.foreach(tag => it isTagged tag)
      (1 to randomInt(5)).foreach(_ =>
        it has Strength(description = description)
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Weakness(description = description)
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Opportunity(description = description)
      )
      (1 to randomInt(5)).foreach(_ => it has Threat(description = description))
      it isIdentifiedAs id on name
      it isIdentifiedAs id on name
      it should verdict
    }

  def capabilityHierarchy(
      servedEnterprise: Option[Enterprise] = None,
      parentCapability: Option[BusinessCapability] = None,
      maxLevel: Int = 0,
      currentLevel: Int = 0
  ): List[BusinessCapability] = if (currentLevel > maxLevel) Nil
  else
    (1 to randomInt(5)).toList.flatMap(it => {
      val cap = ea describes BusinessCapability() as { it =>
        it has Title(s"${parentCapability.map(_.title).getOrElse("")}${it}")
        it has Description(description)
        it has Description(description)
        if (servedEnterprise.isDefined) it serves servedEnterprise.get
        if (parentCapability.isDefined) it serves parentCapability.get
      }
      cap :: capabilityHierarchy(None, Some(cap), maxLevel, currentLevel + 1)
    })

  def buildingBlock(
      realizedCapability: Option[BusinessCapability] = None
  ): ArchitectureBuildingBlock =
    ea describes ArchitectureBuildingBlock() as { it =>
      it has Title(title)
      it has Description(description)
      if (realizedCapability.isDefined) it realizes realizedCapability.get
    }

  def platform(
      realizedBuildingBlock: Option[ArchitectureBuildingBlock] = None,
      name: Option[String] = None
  ): ItPlatform =
    ea describes ItPlatform() as { it =>
      it has Title(name.getOrElse(title))
      it has Description(description)
      if (realizedBuildingBlock.isDefined) it realizes realizedBuildingBlock.get
    }

  def platformLayer: PlatformLayer =
    ea describes PlatformLayer(color = Color.random) as { it =>
      it has Title(title)
      it has Description(description)
    }

  def project(forEnterprise: Option[Enterprise] = None): ItProject = {
    val theProject = ea describes ItProject() as { it =>
      it has Title(title)
      if (forEnterprise.nonEmpty) it serves forEnterprise.get
      (1 to randomInt(5)).foreach(_ => it has Description(description))

    }
    (1 to randomInt(5)).foreach(_ =>
      projectMilestone(forProject = theProject, forEnterprise = forEnterprise)
    )
    theProject
  }

  def projectMilestone(
      forProject: ItProject,
      forEnterprise: Option[Enterprise] = None
  ): ItProjectMilestone = {
    val stakeholders = (1 to randomInt(10)).map(_ => person(forEnterprise))
    val responsible = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val accountable = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val consulted = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val informed = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val technologies = (1 to randomInt(5)).map(_ => language)
    val theCapability = capability(forEnterprise)
    val theBuildingBlock = buildingBlock(Some(theCapability))
    val systems = (1 to randomInt(10) + 2).map(_ =>
      system(realizedBuildingBlock = Some(theBuildingBlock))
    )

    val integrations = systems.tail.map(s => integration(s, systems.head))

    val illustration = ea needs FlowView() and { it =>
      systems.foreach(system =>
        it has Request(title) from system to systems.head
      )
    }

    ea describes ItProjectMilestone(
    ) as { it =>
      it has Title(title)
      it isPartOf forProject
      (1 to randomInt(5)).foreach(_ => it has Description(description))
      (1 to randomInt(5)).foreach(_ =>
        it has CurrentState(
          description = description,
          illustratedBy = Some(illustration)
        )
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Assumption(
          description = description,
          illustratedBy = Some(illustration)
        )
      )
      (1 to randomInt(5)).foreach(_ =>
        it has OpenQuestion(
          description = description,
          illustratedBy = Some(illustration)
        )
      )
      (1 to randomInt(5)).foreach(_ =>
        it has KPI(
          description = description,
          title = title
        )
      )
      (1 to randomInt(5)).foreach(_ => it has Goal(description = description))
      (1 to randomInt(5)).foreach(_ =>
        it has Consequence(
          description = description,
          illustratedBy = Some(illustration)
        )
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Solution(
          description = description,
          illustratedBy = Some(illustration)
        )
      )
      (1 to randomInt(5)).foreach(_ =>
        it has CounterMeasure(
          description = description,
          against = description,
          illustratedBy = Some(illustration)
        )
      )
      (1 to randomInt(4)).foreach(_ => it has Website(url = url, title = name))
      (1 to randomInt(3)).foreach(_ => it has Wiki(url = url, title = name))

      (0 to randomInt(3)).foreach(yearIndex => {
        val year = SomeYear(ThisYear.value + yearIndex)
        (1 to randomInt(10)).foreach(_ =>
          it costs Capex(
            title = title,
            description = title,
            category = Category(Some(title)),
            fiscalYear = year,
            numberOfUnits = unitCount,
            unitOfMeasure = unitOfMeasure,
            costPerUnit = monetaryAmount
          )
        )
        (1 to randomInt(10)).foreach(_ =>
          it costs Opex(
            title = title,
            description = title,
            category = Category(Some(title)),
            fiscalYear = year,
            numberOfUnits = unitCount,
            unitOfMeasure = unitOfMeasure,
            costPerUnit = monetaryAmount
          )
        )

      })

      val functionalRequirements = (1 to randomInt(5)).map(_ =>
        it has FunctionalRequirement(title = title, description = description)
      )
      val qar = (1 to randomInt(5)).map(_ =>
        it has QualityAttributeRequirement(
          title = title,
          sourceOfStimulus = name,
          stimulus = description,
          environment = title,
          response = description,
          responseMeasure = title
        )
      )

      val constraints = (1 to randomInt(5)).map(_ =>
        it has Constraint(title = title, description = description)
      )
      (1 to randomInt(5)).foreach(_ => it has Website(url = url))
      it dealsWith PrivacyCompliance(description)
      it dealsWith PCICompliance(description)
      it dealsWith HealthDataCompliance(description)
      it has HighImpact on Confidentiality(description = description)
      it has LowImpact on Availability(description = description)

      stakeholders.foreach(them => it hasStakeholder them)
      responsible.foreach(them => it isResponsibilityOf them)
      accountable.foreach(them => it isAccountabilityOf them)
      consulted.foreach(them => it hasConsulted them)
      informed.foreach(them => it hasInformed them)

      technologies.foreach(t => it changes t)
      it isCreating theCapability and { that =>
        that has Description(title)
      }
      it keeps theBuildingBlock
      systems.foreach(s =>
        it isChanging s and { that => that has Description(title) }
      )
      it isCreating integrations.head and { that =>
        that has Description(title)
      }
      it isRemoving integrations.tail.head and { that =>
        that has Description(title)
      }
      integrations.tail.tail.foreach(i => it changes i)
    }
  }

  def decision(
      forEnterprise: Option[Enterprise] = None,
      status: DecisionStatus = NotStarted
  ): Decision = {
    val stakeholders = (1 to randomInt(10)).map(_ => person(forEnterprise))
    val responsible = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val accountable = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val consulted = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val informed = (1 to randomInt(5)).map(_ => person(forEnterprise))
    val technologies = (1 to randomInt(5)).map(_ => language)
    val theCapability = capability(forEnterprise)
    val theBuildingBlock = buildingBlock(Some(theCapability))
    val systems = (1 to randomInt(3)).map(_ =>
      system(realizedBuildingBlock = Some(theBuildingBlock))
    )
    val principles = (1 to randomInt(5)).map(_ => principle(forEnterprise))

    val theDecision = ea describes Decision(
      outcome = description,
      status = status
    ) as { it =>
      it has Title(title)
      (1 to randomInt(5)).foreach(_ => it has Description(description))
      (1 to randomInt(5)).foreach(_ =>
        it has CurrentState(description = description, title = title)
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Assumption(description = description, title = title)
      )
      (1 to randomInt(5)).foreach(_ =>
        it has OpenQuestion(description = description, title = title)
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Goal(description = description, title = title)
      )
      (1 to randomInt(5)).foreach(_ =>
        it has Consequence(description = description, title = title)
      )
      (1 to randomInt(4)).foreach(_ => it has Website(url = url, title = name))
      (1 to randomInt(3)).foreach(_ => it has Wiki(url = url, title = name))
      val functionalRequirements = (1 to randomInt(5)).map(_ =>
        it has FunctionalRequirement(title = title, description = description)
      )
      val qar = (1 to randomInt(5)).map(_ =>
        it has QualityAttributeRequirement(
          title = title,
          sourceOfStimulus = name,
          stimulus = description,
          environment = title,
          response = description,
          responseMeasure = title
        )
      )

      val constraints = (1 to randomInt(5)).map(_ =>
        it has Constraint(title = title, description = description)
      )
      (1 to randomInt(5)).foreach(_ => it has Website(url = url))
      it dealsWith PrivacyCompliance(description)
      it dealsWith PCICompliance(description)
      it dealsWith HealthDataCompliance(description)
      it has HighImpact on Confidentiality(description = description)
      it has MediumImpact on Integrity(description = description)
      it has LowImpact on Availability(description = description)

      stakeholders.foreach(them => it hasStakeholder them)
      responsible.foreach(them => it isResponsibilityOf them)
      accountable.foreach(them => it isAccountabilityOf them)
      consulted.foreach(them => it hasConsulted them)
      informed.foreach(them => it hasInformed them)

      principles.foreach(them => it isInfluencedBy them)

      technologies.foreach(t => it changes t)
      it creates theCapability
      it keeps theBuildingBlock
      systems.foreach(s => it changes s)
    }

    debug(s"$theDecision")

    (1 to randomInt(6)).foreach(_ =>
      ea describes DecisionOption() as { option =>
        option isPartOf theDecision
        (1 to randomInt(5)).foreach(_ => option has Description(description))
        (1 to randomInt(5)).foreach(_ => option has Website(url = url))
        (1 to randomInt(5)).foreach(_ =>
          option has Strength(description = description)
        )
        (1 to randomInt(5)).foreach(_ =>
          option has Weakness(description = description)
        )
        (1 to randomInt(5)).foreach(_ =>
          option has Opportunity(description = description)
        )
        (1 to randomInt(5)).foreach(_ =>
          option has Threat(description = description)
        )
        (0 to randomInt(3)).foreach(yearIndex => {
          val year = SomeYear(ThisYear.value + yearIndex)
          (1 to randomInt(10)).foreach(_ =>
            option costs Capex(
              title = title,
              description = title,
              category = Category(Some(title)),
              fiscalYear = year,
              numberOfUnits = unitCount,
              unitOfMeasure = unitOfMeasure,
              costPerUnit = monetaryAmount
            )
          )
          (1 to randomInt(10)).foreach(_ =>
            option costs Opex(
              title = title,
              description = title,
              category = Category(Some(title)),
              fiscalYear = year,
              numberOfUnits = unitCount,
              unitOfMeasure = unitOfMeasure,
              costPerUnit = monetaryAmount
            )
          )

        })

        theDecision.functionalRequirements.foreach(f =>
          option scores MeetsExpectations(description =
            description
          ) on f.key.value of theDecision
        )
        theDecision.qualityAttributeRequirements.foreach(f =>
          option scores DoesNotMeetExpectations(description =
            description
          ) on f.key.value of theDecision
        )
        theDecision.constraints.foreach(f =>
          option scores ExceedsExpectations(description =
            description
          ) on f.key.value of theDecision
        )
      }
    )

    theDecision
  }

  def person(forEnterprise: Option[Enterprise] = None): Person =
    ea describes Person() as { it =>
      if (forEnterprise.isDefined) it serves forEnterprise.get
      it has Title(name)
      it has Description(description)
    }

  def principle(forEnterprise: Option[Enterprise] = None): Principle =
    ea describes DesignPrinciple() as { it =>
      it has Title(title)
      it has Description(description)
      if (forEnterprise.isDefined) it serves forEnterprise.get
    }

  def system(
      withContainers: Boolean = true,
      containingPlatform: Option[ItPlatform] = None,
      realizedBuildingBlock: Option[ArchitectureBuildingBlock] = None,
      fatherTime: Set[FatherTime] = Set(),
      onPlatformLayer: Option[PlatformLayer] = None,
      tags: List[Tag] = Nil,
      name: Option[String] = None
  ): ItSystem = {
    val theSystem = ea describes ItSystem() as { it =>
      it has Title(name.getOrElse(title))
      it has Description(description)

      if (containingPlatform.isDefined) {
        it isPartOf containingPlatform.get
      }

      if (realizedBuildingBlock.isDefined) {
        it realizes realizedBuildingBlock.get
      }

      fatherTime.foreach(f => it has f on f.date)

      onPlatformLayer.foreach(pl => it isOn pl)

      tags.foreach(tag => it isTagged tag)
    }

    if (withContainers) {
      val ms1 = microservice(theSystem)
      val ms2 = microservice(theSystem)
      val u = ui(theSystem)
      val q = queue(theSystem)
      val d = database(theSystem)

      ea hasRelationship Flow(source = u.key, target = ms1.key)
        .withTitle(Title(title))
        .asInstanceOf[Flow]
      ea hasRelationship Flow(source = ms1.key, target = ms2.key)
        .withTitle(Title(title))
        .asInstanceOf[Flow]
      ea hasRelationship Flow(source = ms1.key, target = d.key)
        .withTitle(Title(title))
        .asInstanceOf[Flow]
      ea hasRelationship Flow(source = ms2.key, target = q.key)
        .withTitle(Title(title))
        .asInstanceOf[Flow]
    }

    theSystem
  }

  def flow(
      source: Element,
      target: Element,
      name: Option[String] = None
  ): Relationship =
    ea hasRelationship Flow(
      source = source.key,
      target = target.key
    ).withTitle(Title(name.getOrElse(title))).asInstanceOf[Flow]

  def integration(
      system1: ItSystem,
      system2: ItSystem,
      fatherTime: Set[FatherTime] = Set()
  ): ItSystemIntegration = {
    val illustration1: FlowView = ea needs FlowView() and { it =>
      it has Title(title)
      it has Request(title) from system1 to system2
      it has Response(title) from system2 to system1
    }

    val illustration2: FlowView = ea needs FlowView() and { it =>
      it has Title(title)
      it has Message(title) from system1 to system2
    }

    ea describes ItSystemIntegration() between system1 and system2 as { it =>
      it has Title(title)
      it has Description(description)
      fatherTime.foreach(f => it has f on f.date)

      it has Request(
        title
      ) containing "some JSON" using "REST" from system1 to system2
      it has Response(title) containing "some XML" from system2 to system1
      it has Message(title) from system1 to system2

      it isIllustratedBy illustration1
      it isIllustratedBy illustration2

    }
  }

  def goneToProduction(year: Int, month: Int, day: Int): GoneToProduction =
    GoneToProduction(date = Day(year, month, day), description = description)

  def startedDevelopment(year: Int, month: Int, day: Int): StartedDevelopment =
    StartedDevelopment(date = Day(year, month, day), description = description)

  def decommissioned(year: Int, month: Int, day: Int): Decommissioned =
    Decommissioned(date = Day(year, month, day), description = description)

  def entity: Entity = ea describes Entity() as { it =>
    it has Title(title)
    it has Description(description)
    it isClassified PublicData
    (1 to randomInt(10)).foreach(_ =>
      it has DataAttribute(
        name = title,
        description = Some(description),
        required = true,
        multiple = false,
        dataType = Some(word)
      )
    )
  }

  def aggregateRoot: AggregateRoot =
    ea describes AggregateRoot() as { it =>
      it has Title(title)
      it has Description(description)
      it isClassified PersonalData
      (1 to randomInt(10)).foreach(_ =>
        it has DataAttribute(
          name = title,
          description = Some(description),
          required = true,
          multiple = false,
          dataType = Some(word)
        )
      )
    }

  def valueObject: ValueObject = ea describes ValueObject() as { it =>
    it has Title(title)
    it has Description(description)
    it isClassified SensitiveData
    (1 to randomInt(10)).foreach(_ =>
      it has DataAttribute(
        name = title,
        description = Some(description),
        required = true,
        multiple = false,
        dataType = Some(word)
      )
    )
  }

  def event: Event = ea describes Event() as { it =>
    it has Title(title)
    it has Description(description)
    it isClassified ConfidentialData
    (1 to randomInt(3)).foreach(_ =>
      it has DataAttribute(
        name = title,
        description = Some(description),
        required = true,
        multiple = false,
        dataType = Some(word)
      )
    )
  }

  def command: Command = ea describes Command() as { it =>
    it has Title(title)
    it has Description(description)
    it isClassified PublicData
    (1 to randomInt(3)).foreach(_ =>
      it has DataAttribute(
        name = title,
        description = Some(description),
        required = true,
        multiple = false,
        dataType = Some(word)
      )
    )
  }

  def query: Query = ea describes Query() as { it =>
    it has Title(title)
    it has Description(description)
    (1 to randomInt(3)).foreach(_ =>
      it has DataAttribute(
        name = title,
        description = Some(description),
        required = true,
        multiple = false,
        dataType = Some(word)
      )
    )
  }

  def projection: Projection = ea describes Projection() as { it =>
    it has Title(title)
    it has Description(description)
    (1 to randomInt(3)).foreach(_ =>
      it has DataAttribute(
        name = title,
        description = Some(description),
        required = true,
        multiple = false,
        dataType = Some(word)
      )
    )
  }

  def technicalDebt(
      appliesTo: List[CanBeAssociated] = Nil,
      name: Option[String] = None
  ): Risk = risk(appliesTo, TechnicalDebt, name)
  def securityVulnerability(
      appliesTo: List[CanBeAssociated] = Nil,
      name: Option[String] = None
  ): Risk = risk(appliesTo, SecurityVulnerability, name)
  def complianceRisk(
      appliesTo: List[CanBeAssociated] = Nil,
      name: Option[String] = None
  ): Risk = risk(appliesTo, ComplianceRisk, name)

  def risk(
      appliesTo: List[CanBeAssociated] = Nil,
      typeOfRisk: TypeOfRisk,
      name: Option[String] = None
  ): Risk = ea describes Risk(
    typeOfRisk = typeOfRisk
  ) as { it =>
    it has Title(name.getOrElse(title))
    it has Description(description)
    it has CurrentState(description = description)
    it has Consequence(description = description)
    it has CounterMeasure(description = description)
    it ratesImpactAs Hazardous()
    appliesTo.foreach(that => it isAssociatedWith that)
  }

  def access(source: CanAccess, target: CanBeAccessed): Relationship =
    ea hasRelationship Accessing(source = source.key, target = target.key)

  def consume(source: CanConsume, target: CanBeConsumed): Relationship =
    ea hasRelationship Consuming(source = source.key, target = target.key)

  def own(source: CanOwn, target: CanBeOwned): Relationship =
    ea hasRelationship Owning(source = source.key, target = target.key)

  def process(source: CanProcess, target: CanBeProcessed): Relationship =
    ea hasRelationship Processing(source = source.key, target = target.key)

  def produce(source: CanProduce, target: CanBeProduced): Relationship =
    ea hasRelationship Producing(source = source.key, target = target.key)

  def transport(source: CanTransport, target: CanBeTransported): Relationship =
    ea hasRelationship Transporting(source = source.key, target = target.key)

  def dataRelationship(
      source: CanHaveDataRelationship,
      target: CanHaveDataRelationship
  ): Relationship =
    ea hasRelationship DataRelationship(
      source = source.key,
      target = target.key,
      left = One,
      right = OneOrMore
    )

}
