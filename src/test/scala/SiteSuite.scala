package com.alecdorrington.codeswitch

import io.circe.Json
import munit.FunSuite

class SiteSuite extends FunSuite:

  /** The site of `address`, which must be one. */
  private def site(address: String): Site = Site.parse(address).get

  /** Whether `address`, as a site, covers the page at each of `pages`. */
  private def covers(address: String, pages: String*): List[Boolean] = pages
    .toList
    .map(Site.covered(List(site(address)), _))

  test(
    "A site is read from an address typed or pasted, with or without its scheme, without a leading `*.` or `www.`, its fragment, or a slash at its end.",
  ):
    assertEquals(
      Site.parse("example.com"),
      Some(Site("example.com", "", Nil, "")),
    )
    assertEquals(
      Site.parse("  https://www.Example.COM/Docs/Guide/?page=2#top "),
      Some(Site(
        "example.com",
        "",
        List("Docs", "Guide"),
        "?page=2",
      )),
    )
    assertEquals(
      site("*.example.com"),
      site("example.com"),
    )
    assertEquals(
      site("http://localhost:8080/"),
      Site("localhost", "8080", Nil, ""),
    )
    assertEquals(
      site("https://example.com:443"),
      site("example.com"),
    )
    assertEquals(
      site("user:secret@example.com"),
      site("example.com"),
    )
    assertEquals(
      List(
        "my_host.local",
        "192.168.0.1",
        "[::1]:8080",
      ).map(site(_).host),
      List("my_host.local", "192.168.0.1", "[::1]"),
    )

  test("Nothing that is not a website's address is read as one."):
    List(
      "",
      "   ",
      "not a website",
      "file:///home/notes.txt",
      "chrome://extensions",
      "not%20a%20website",
      "ex!ample.com",
      "example.com:8080:9090",
      "https://",
    ).foreach(text => assertEquals(Site.parse(text), None, text))

  test(
    "A domain covers its subdomains, but not another domain that ends in its name.",
  ):
    assertEquals(
      covers(
        "example.com",
        "https://example.com/",
        "http://www.example.com/page",
        "https://news.example.com/2026/10/31?id=1#comments",
        "https://a.b.example.com",
        "https://notexample.com/",
        "https://example.com.au/",
        "https://example.org/",
      ),
      List(true, true, true, true, false, false, false),
    )
    assertEquals(
      covers(
        "news.example.com",
        "https://news.example.com/",
        "https://sport.news.example.com/",
        "https://example.com/",
      ),
      List(true, true, false),
    )

  test(
    "A path covers the pages under it, but not another that begins as it does.",
  ):
    assertEquals(
      covers(
        "example.com/docs",
        "https://example.com/docs",
        "https://example.com/docs/",
        "https://www.example.com/docs/guide/intro?x=1",
        "https://api.example.com/docs/guide",
        "https://example.com/",
        "https://example.com/docsy",
        "https://example.com/Docs",
        "https://example.com/blog/docs",
      ),
      List(true, true, true, true, false, false, false, false),
    )

  test(
    "A query covers only the pages with each of its parameters, whatever others they have.",
  ):
    assertEquals(
      covers(
        "youtube.com/watch?v=abc",
        "https://www.youtube.com/watch?v=abc",
        "https://www.youtube.com/watch?t=42&v=abc",
        "https://www.youtube.com/watch?v=xyz",
        "https://www.youtube.com/watch",
      ),
      List(true, true, false, false),
    )

  test(
    "A port covers only its pages, but a site without one covers every port.",
  ):
    assertEquals(
      covers(
        "localhost:3000",
        "http://localhost:3000/",
        "http://localhost:8080/",
        "http://localhost/",
      ),
      List(true, false, false),
    )
    assertEquals(
      covers(
        "localhost",
        "http://localhost:3000/",
        "http://localhost/",
      ),
      List(true, true),
    )

  test(
    "An address in another script is read as the browser gives it, and shown as it was typed.",
  ):
    val wiki = site("de.wikipedia.org/wiki/Über")
    assertEquals(
      wiki.address,
      "de.wikipedia.org/wiki/%C3%9Cber",
    )
    assertEquals(
      wiki.text,
      "de.wikipedia.org/wiki/Über",
    )
    assertEquals(
      covers(
        "de.wikipedia.org/wiki/Über",
        "https://de.wikipedia.org/wiki/%C3%9Cber",
      ),
      List(true),
    )
    assertEquals(
      site("bücher.de").host,
      "xn--bcher-kva.de",
    )

  test("A site's domain has neither its path nor its query."):
    assertEquals(
      site("https://www.example.com:8443/docs/guide?page=2").domain,
      site("example.com:8443"),
    )

  test(
    "A site is kept as its address, read back as it was, and one that cannot be read is left out of the list.",
  ):
    val sites = List(
      site("example.com"),
      site("localhost:3000/app"),
      site("youtube.com/watch?v=abc"),
      site("de.wikipedia.org/wiki/Über"),
    )
    val codec = Codeswitch.blocked.codec
    assertEquals(
      codec(sites),
      Json.arr(
        Json.fromString("example.com"),
        Json.fromString("localhost:3000/app"),
        Json.fromString("youtube.com/watch?v=abc"),
        Json.fromString("de.wikipedia.org/wiki/%C3%9Cber"),
      ),
    )
    assertEquals(
      codec.decodeJson(codec(sites)),
      Right(sites),
    )
    assertEquals(
      codec.decodeJson(Json.arr(
        Json.fromString("example.com"),
        Json.fromString("not a website"),
        Json.fromInt(1),
      )),
      Right(List(site("example.com"))),
    )
    assertEquals(
      Codeswitch.allowed.codec(sites),
      codec(sites),
    )
    assertEquals(
      (Codeswitch.blocked.default, Codeswitch.allowed.default),
      (Nil, Nil),
    )

  test(
    "Every website but those blocked has anything marked, unless only those allowed are to, and the choice is kept by its name, each with its own list.",
  ):
    assertEquals(
      Scope
        .values
        .toList
        .map(scope => (scope.leaves(true), scope.leaves(false))),
      List((true, false), (false, true)),
    )
    val codec = Codeswitch.scope.codec
    assertEquals(
      Scope.values.toList.map(codec(_)),
      List(
        Json.fromString("Except"),
        Json.fromString("Only"),
      ),
    )
    assertEquals(
      Scope.values.toList.map(scope => codec.decodeJson(codec(scope))),
      Scope.values.toList.map(Right(_)),
    )
    assertEquals(Codeswitch.scope.default, Scope.Except)
    assertEquals(
      Scope.values.toList.map(Codeswitch.listOf),
      List(Codeswitch.blocked, Codeswitch.allowed),
    )
