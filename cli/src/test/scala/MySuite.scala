// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class CLISuite extends munit.FunSuite {
  test("No tests") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
}
