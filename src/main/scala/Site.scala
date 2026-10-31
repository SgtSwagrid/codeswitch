package com.alecdorrington.codeswitch

import io.circe.{Codec, Decoder, Encoder}
import org.scalajs.dom.{URL, URLSearchParams}
import scala.scalajs.js.URIUtils.decodeURI
import scala.util.Try

/**
  * A website, or a part of one, as an address without its scheme: a domain,
  * which covers its subdomains, and any path, which covers the pages under it,
  * and query, whose parameters the pages it covers must have too.
  *
  * @param host
  *   The domain, in lower case and in ASCII, without a leading `www.`.
  * @param port
  *   The port, if the address gives one other than its scheme's own, or else
  *   `""`.
  * @param path
  *   The segments of the path, percent-encoded, as an address has them.
  * @param query
  *   The query, beginning with `?`, percent-encoded, or else `""`.
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
    * at this port, if this gives one, and at this path, or one under it, with
    * every parameter of this query.
    */
  def covers(page: Site): Boolean =
    (page.host == host || page.host.endsWith(s".$host")) &&
    (port.isEmpty || page.port == port) && page.path.startsWith(path) &&
    parameters.forall(page.parameters.contains)

  /** This domain, at this port, with no path or query. */
  def domain: Site = copy(path = Nil, query = "")

  /** This, as it is kept: its address, without a scheme, percent-encoded. */
  def address: String = host + (if port.isEmpty then "" else s":$port") +
    path.map("/" + _).mkString + query

  /** This, as the reader reads it: its address, decoded where it may be. */
  def text: String = Try(decodeURI(address)).getOrElse(address)

  /** The parameters of this query, by name. */
  private def parameters: List[(String, String)] = URLSearchParams(query)
    .toList
    .map(pair => (pair._1, pair._2))

object Site:

  /**
    * The site at `address`, typed or pasted, with or without its scheme (`http`
    * or `https`), if it is one: any `*.` that leads it, any `www.` that leads
    * its domain, and any fragment, are left out.
    */
  def parse(address: String): Option[Site] = Some(
    address.trim.stripPrefix("*."),
  ).filter(_.nonEmpty)
    .map(text => if text.contains("://") then text else s"https://$text")
    .flatMap(text => Try(URL(text)).toOption)
    .filter(url => Set("http:", "https:")(url.protocol) && named(url.hostname))
    .map(url =>
      Site(
        url.hostname.stripPrefix("www."),
        url.port,
        url.pathname.split('/').toList.filter(_.nonEmpty),
        url.search,
      ),
    )

  /**
    * Whether `host` is a domain's name, in ASCII, as the browser gives it, or
    * an IPv6 address in brackets, rather than any other text that a browser
    * takes for one, as Chrome takes `not a website`, its spaces encoded.
    */
  private def named(host: String): Boolean =
    host.matches("""[a-z0-9_-]+(\.[a-z0-9_-]+)*|\[[0-9a-f:.]+\]""")

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
