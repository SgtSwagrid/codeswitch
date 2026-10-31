package com.alecdorrington.codeswitch

import io.circe.{Codec, Decoder, Encoder}
import org.scalajs.dom.{URL, URLSearchParams}
import scala.annotation.tailrec
import scala.scalajs.js.URIUtils.decodeURI
import scala.util.Try
import scala.util.matching.Regex

/**
  * A website, or a part of one, as an address without its scheme: a domain,
  * which covers its subdomains, and any path, which covers the pages under it,
  * and query, whose parameters the pages it covers must have too.
  *
  * @param host
  *   The domain, in lower case and in ASCII, without a dot at its end.
  * @param port
  *   The port, if the address gives one other than `80` or `443`, or else `""`.
  * @param path
  *   The segments of the path, percent-encoded, as an address has them, each
  *   code in upper case.
  * @param query
  *   The query, beginning with `?`, percent-encoded as the path is, or else
  *   `""`.
  */
final case class Site
  (
    host: String,
    port: String,
    path: List[String],
    query: String,
  ):

  /**
    * Whether this covers `page`: whether it is on this domain, or one under it,
    * or this domain without its `www.`, at this port, if this gives one, and at
    * this path, or one under it, with every parameter of this query.
    */
  def covers(page: Site): Boolean =
    (page.host == host || page.host.endsWith(s".$host") ||
      host == s"www.${ page.host }") && (port.isEmpty || page.port == port) &&
    page.path.startsWith(path) && parameters.forall(page.parameters.contains)

  /** This domain, at this port, with no path or query. */
  def domain: Site = copy(path = Nil, query = "")

  /** This, as it is kept: its address, without a scheme, percent-encoded. */
  def address: String = host + after

  /**
    * This, as the reader reads it: its address, its domain in its own script
    * and the rest decoded where it may be.
    */
  def text: String = host.split('.').map(Punycode.label).mkString(".") +
    Try(decodeURI(after)).getOrElse(after)

  /** This address after its domain: its port, path and query. */
  private def after: String = (if port.isEmpty then "" else s":$port") +
    path.map("/" + _).mkString + query

  /** The parameters of this query, by name. */
  private def parameters: List[(String, String)] = URLSearchParams(query)
    .toList
    .map(pair => (pair._1, pair._2))

object Site:

  /**
    * The site at `address`, typed or pasted, with or without its scheme (`http`
    * or `https`, even missing a slash), if it is one: any `*.` that leads its
    * domain, any dot that ends it, its credentials and any fragment are left
    * out.
    */
  def parse(address: String): Option[Site] = Some(address.trim)
    .filter(_.nonEmpty)
    .map(schemed)
    .flatMap(text => Try(URL(text)).toOption)
    .filter(url => Set("http:", "https:")(url.protocol))
    .map(url => (url, url.hostname.stripSuffix(".")))
    .filter((_, host) => named(host))
    .map((url, host) =>
      Site(
        host,
        if Set("80", "443")(url.port) then "" else url.port,
        url.pathname.split('/').toList.filter(_.nonEmpty).map(upper),
        upper(url.search),
      ),
    )

  /**
    * `text` with the scheme `https`, in place of `http` or `https` with any
    * number of slashes, or before it if it has none, any `*.` before its domain
    * left out; or as it is, with any other scheme.
    */
  private def schemed(text: String): String = text match
    case Web(rest) => s"https://${ rest.stripPrefix("*.") }"
    case Other()   => text
    case _         => s"https://${ text.stripPrefix("*.") }"

  /** An address of the web, `http` or `https`, and what follows its slashes. */
  private val Web = """(?i)https?:/*(.*)""".r

  /** An address of another scheme. */
  private val Other = """(?i)[a-z][a-z0-9+.-]*://.*""".r

  /**
    * Whether `host` is a domain's name, in ASCII, as the browser gives it, or
    * an IPv6 address in brackets, rather than any other text that a browser
    * takes for one, as Chrome takes `not a website`, its spaces encoded, or the
    * scheme alone, as of the typo `https//example.com`.
    */
  private def named(host: String): Boolean =
    host.matches("""[a-z0-9_-]+(\.[a-z0-9_-]+)*|\[[0-9a-f:.]+\]""") &&
    !Set("http", "https")(host)

  /** `text` with each code of its percent-encoding in upper case. */
  private def upper(text: String): String = """%[0-9a-fA-F]{2}"""
    .r
    .replaceAllIn(
      text,
      code => Regex.quoteReplacement(code.matched.toUpperCase),
    )

  /** Whether any of `sites` covers the page at `address`. */
  def covered(sites: List[Site], address: String): Boolean = parse(address)
    .exists(page => sites.exists(_.covers(page)))

  /** A site, kept as its address, which must be one. */
  given Codec[Site] = Codec.from(
    Decoder[String].emap(address =>
      parse(address).toRight(s"$address is not a website."),
    ),
    Encoder[String].contramap(_.address),
  )

/**
  * RFC 3492's Punycode, in which a domain's labels in another script than
  * Latin's are written in ASCII, after `xn--`, as browsers give them.
  */
private object Punycode:

  private val base        = 36
  private val tMin        = 1
  private val tMax        = 26
  private val skew        = 38
  private val damp        = 700
  private val initialBias = 72
  private val initialN    = 128

  /** `label` in its own script, if it is in Punycode, or else as it is. */
  def label(label: String): String =
    if label.startsWith("xn--") then decode(label.drop(4)).getOrElse(label)
    else label

  /** The text that `encoded` stands for, if it is Punycode. */
  def decode(encoded: String): Option[String] =
    val split = encoded.lastIndexOf('-')
    val basic = if split < 0 then "" else encoded.take(split)
    Option
      .when(basic.forall(_ < 0x80))(encoded.drop(split + 1).toList)
      .flatMap(digits =>
        inserted(
          digits,
          basic.map(_.toInt).toVector,
          initialN,
          0,
          initialBias,
        ),
      )
      .map(_.map(point => Character.toChars(point).mkString).mkString)

  /**
    * `output`, with each code point that `digits` stand for inserted, as from
    * the code point `n`, at the place `i`, under `bias`.
    */
  @tailrec
  private def inserted
    (
      digits: List[Char],
      output: Vector[Int],
      n: Int,
      i: Int,
      bias: Int,
    )
    : Option[Vector[Int]] = digits match
    case Nil => Some(output)
    case _   => integer(digits, bias) match
        case None                => None
        case Some((delta, rest)) =>
          val size  = output.length + 1
          val total = i + delta
          val point = n + total / size
          val at    = total % size
          inserted(
            rest,
            output.patch(at, Vector(point), 0),
            point,
            at + 1,
            adapt(delta, size, i == 0),
          )

  /**
    * The variable-length integer that `digits` begin with, under `bias`, and
    * the digits after it, if they hold one.
    */
  private def integer
    (
      digits: List[Char],
      bias: Int,
      k: Int = base,
      weight: Int = 1,
      sum: Int = 0,
    )
    : Option[(Int, List[Char])] = digits match
    case Nil          => None
    case char :: rest => value(char).flatMap(digit =>
        val total     = sum + digit * weight
        val threshold =
          if k <= bias then tMin
          else if k >= bias + tMax then tMax
          else k - bias
        if digit < threshold then Some((total, rest))
        else
          integer(
            rest,
            bias,
            k + base,
            weight * (base - threshold),
            total,
          ),
      )

  /**
    * The value of the digit `char`: `a` to `z` are `0` to `25`, `0` to `9` the
    * rest.
    */
  private def value(char: Char): Option[Int] = char.toLower match
    case letter if letter >= 'a' && letter <= 'z' => Some(letter - 'a')
    case digit if digit >= '0' && digit <= '9'    => Some(digit - '0' + 26)
    case _                                        => None

  /** The bias after `delta`, of `points` code points, the `first` or not. */
  private def adapt(delta: Int, points: Int, first: Boolean): Int =
    val scaled = if first then delta / damp else delta / 2
    settle(scaled + scaled / points, 0)

  @tailrec
  private def settle(delta: Int, k: Int): Int =
    if delta > ((base - tMin) * tMax) / 2 then
      settle(delta / (base - tMin), k + base)
    else k + ((base - tMin + 1) * delta) / (delta + skew)

/**
  * Which websites have anything marked: every one but those the reader blocks,
  * or only those they allow, each kept in a list of its own.
  */
enum Scope:

  /** Every website but those blocked. */
  case Except

  /** Only the websites allowed. */
  case Only

  /**
    * Whether a page is left alone under this scope, if its list covers it, as
    * `listed` says.
    */
  def leaves(listed: Boolean): Boolean = this match
    case Except => listed
    case Only   => !listed

object Scope:

  /** A scope, kept by its name, as `Except`. */
  given Codec[Scope] = Codec.from(
    Decoder[String].emap(name =>
      values.find(_.toString == name).toRight(s"Nothing is $name."),
    ),
    Encoder[String].contramap(_.toString),
  )
