package fdm

/**
 * Sometimes we don't want to take the time to model data precisely. For example, we might want to
 * model an email address with a string, even though most strings are not valid email addresses.
 *
 * In such cases, we can save time by using a smart constructor, which lets us ensure we model
 * only valid data, but without complicated data types.
 */
object smart_constructors {
  sealed abstract case class Email private (value: String)
  object Email {
    def fromString(email: String): Option[Email] =
      if (email.matches("""/\w+@\w+.com""")) Some(new Email(email) {}) else None
  }

  /**
   * EXERCISE 1
   *
   * Create a smart constructor for `NonNegative` which ensures the integer is always non-negative.
   */
  sealed abstract case class NonNegative private (value: Int)

  /**
   * EXERCISE 2
   *
   * Create a smart constructor for `Age` that ensures the integer is between 0 and 120.
   */
  sealed abstract case class Age private (value: Int)

  /**
   * EXERCISE 3
   *
   * Create a smart constructor for password that ensures some security considerations are met.
   */
  sealed abstract case class Password private (value: String)
}

object applied_smart_constructors {

  sealed trait DomainError

  object DomainError {
    final case class ValidationFailed(why: String) extends DomainError
  }
  import DomainError._

  type ErrorOr[A] = Either[DomainError, A]

  /**
   * EXERCISE 1
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class BankAccount(id: AccountId, name: String, balance: Double, opened: java.time.Instant)

  sealed abstract case class AccountId private (value: String)
  object AccountId {
    def validId(id: String): ErrorOr[AccountId] =
      Either.cond(id.nonEmpty, new AccountId(id) {}, ValidationFailed("wrong id"))
  }
  sealed abstract case class Name private (value: String)
  sealed trait Balance
  object Balance {
    sealed abstract case class USD(dollars: Long) extends Balance
  }

  object BankAccount {

    def make(id: String, name: String, balance: Long) = ???
  }

  /**
   * EXERCISE 2
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class Person(age: Int, name: String, salary: Double)

  /**
   * EXERCISE 3
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class SecurityEvent(machine: String, timestamp: String, eventType: Int)
  object EventType {
    val PortScanning    = 0
    val DenialOfService = 1
    val InvalidLogin    = 2
  }
}
