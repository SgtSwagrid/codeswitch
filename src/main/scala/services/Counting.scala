package com.alecdorrington.codeswitch.services

import cats.effect.{IO, Ref, SyncIO}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Dictionary, Homographs, Pending, Spelling, Store, Timestamp,
}
import com.alecdorrington.codeswitch.models.{whole, Hint, Words}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.Browser
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
  * Counts every word of each passage read in a language being learnt toward the
  * words it may be (see [[FrequencyService]]): toward the dictionary form that
  * a hint of it names, as the model read it in its sentence; or else evenly
  * toward each dictionary form of the lexemes that the lexicon lists its
  * spelling as a form of, as `Bank` is two words but one form, and `vino` both
  * wine and a form of `venir`; or, while the lexicon knows nothing of its
  * spelling, as the spelling waits (see [[Pending]]) to be looked up in
  * Wiktionary, one at a time, at a pace that keeps from burdening it (see
  * [[wake]]), and counted once it has been.
  */
object Counting:

  /** How long it waits between looking spellings up, lest it burden Wiktionary. */
  val pace: FiniteDuration = 1.second

  /** How long after a lookup failed it waits before looking any up again. */
  val rest: FiniteDuration = 1.minute

  /** A word read, `word`, and whether it began its sentence, `opening`. */
  private final case class Token(word: String, opening: Boolean)

  /**
    * Counts the words of `text`, a passage of `language` read for the first
    * time, with `hints` of it: each word that a hint covers toward the
    * dictionary form it names, and the rest by the lexemes of their spellings,
    * those not yet known waiting to be looked up, in `store`, toward
    * `frequencies`.
    */
  def read
    (
      language: Language,
      text: String,
      hints: List[Hint],
      store: Store,
      frequencies: FrequencyService,
    )
    : IO[Unit] =
    val (hinted, rest) = split(language, text, hints)
    attributed(language, rest, store).flatMap((credits, waiting) =>
      frequencies
        .count(language, hinted ++ credits)
        .whenA(hinted.nonEmpty || credits.nonEmpty) *>
        pend(language, waiting, store),
    )

  /**
    * The dictionary forms that `hints` name, each once for the words of `text`,
    * of `language`, that it covers, and the words of `text` that none covers.
    */
  private def split
    (
      language: Language,
      text: String,
      hints: List[Hint],
    )
    : (List[(String, Double)], List[Token]) = hints.foldLeft((
    List.empty[(String, Double)],
    Words
      .findIn(text, language)
      .map(span =>
        Token(
          span.in(text),
          Words.opens(text, span.start),
        ),
      ),
  )) { case ((credits, left), hint) =>
    val form = Some(hint.word.trim)
      .filter(_.nonEmpty)
      .getOrElse(hint.original.trim)
    taken(
      left,
      Words.findIn(hint.original, language).map(_.in(hint.original)),
    ).fold((credits, left))(rest => ((form -> 1.0) :: credits, rest))
  }

  /** `tokens` without one of each of `words`, in any case, if it has them all. */
  private def taken
    (tokens: List[Token], words: List[String])
    : Option[List[Token]] = Option
    .when(words.nonEmpty)(tokens)
    .flatMap(tokens =>
      words.foldLeft(Option(tokens))((left, word) =>
        left.flatMap(left =>
          Some(left.indexWhere(_.word.equalsIgnoreCase(word)))
            .filter(_ >= 0)
            .map(left.patch(_, Nil, 1)),
        ),
      ),
    )

  /**
    * `word` begun in lower case, if it begins with a capital and has no other,
    * as a word that begins a sentence may have one only for that.
    */
  private def lowered(word: String): Option[String] = Option.when(
    word.headOption.exists(_.isUpper) &&
    word.drop(1) == word.drop(1).toLowerCase,
  )(word.take(1).toLowerCase + word.drop(1))

  /**
    * Of `tokens`, words of `language`, how often each dictionary form was read,
    * each word's count shared evenly among those that the lexicon in `store`
    * lists its spelling as a form of, and the spellings that it knows nothing
    * of yet, each with how often it was read, and whether ever at the start of
    * a sentence. A word that begins a sentence is taken, if its spelling is not
    * known as it is written, as it would be begun in lower case.
    */
  private def attributed
    (
      language: Language,
      tokens: List[Token],
      store: Store,
    )
    : IO[(List[(String, Double)], List[(Spelling, Double, Boolean)])] =
    val counted = tokens.groupMapReduce(_.word)(token => (1, token.opening))(
      (first, second) => (first._1 + second._1, first._2 || second._2),
    )
    val spellings = counted
      .toList
      .flatMap((word, counts) =>
        Spelling(language, word) ::
          lowered(word).filter(_ => counts._2).map(Spelling(language, _)).toList,
      )
      .distinct
    for
      listed <- store.homographs(spellings)
      known = spellings
        .zip(listed)
        .filter(_._2.checked.contains(language.whole))
        .toMap
      lemmas <- lemmasOf(language, known.values.toList, store)
    yield counted
      .toList
      .foldMap((word, counts) =>
        val (count, opening) = counts
        val exact            = known.get(Spelling(language, word))
        val lower            =
          lowered(word).filter(_ => opening).map(Spelling(language, _))
        val formed = (exact.toList ++ lower.flatMap(known.get))
          .map(homographs => lemmas.getOrElse(homographs.spelling, Nil))
          .find(_.nonEmpty)
        formed match
          case Some(forms) => (forms.map(_ -> count.toDouble / forms.size), Nil)
          case None if exact.nonEmpty && lower.forall(known.contains) =>
            (Nil, Nil)
          case None =>
            (Nil, List((Spelling(language, word), count.toDouble, opening))),
      )

  /**
    * The dictionary forms of the lexemes of `language` that each of `listed`
    * lists, by the spelling, each once.
    */
  private def lemmasOf
    (
      language: Language,
      listed: List[Homographs],
      store: Store,
    )
    : IO[Map[Spelling, List[String]]] = store
    .lexemes(listed.flatMap(_.lexemes).distinct)
    .map(lexemes =>
      val forms = lexemes
        .filter(_.languages.exists(_.whole == language.whole))
        .map(lexeme => lexeme.id -> lexeme.lemma.spelling.text)
        .toMap
      listed
        .map(homographs =>
          homographs.spelling -> homographs.lexemes.flatMap(forms.get).distinct,
        )
        .toMap,
    )

  /**
    * Keeps each of `waiting`, a spelling of `language` with how often it was
    * read, and whether at the start of a sentence, as waiting to be looked up,
    * added to what waits of it already, one change at a time.
    */
  private def pend
    (
      language: Language,
      waiting: List[(Spelling, Double, Boolean)],
      store: Store,
    )
    : IO[Unit] = Lexicon
    .lock
    .permit
    .surround(
      (
        store.pending(
          waiting.map((spelling, _, _) => Pending.key(language, spelling)),
        ),
        Timestamp.now,
      ).flatMapN((kept, now) =>
        store.pend(
          waiting
            .zip(kept)
            .map { case ((spelling, count, opening), kept) =>
              kept.fold(
                Pending(language, spelling, count, opening, now),
              )(_.more(count, opening))
            },
          Nil,
        ),
      ),
    )
    .whenA(waiting.nonEmpty)

  private val running = Ref.in[SyncIO, IO, Boolean](false).unsafeRunSync()

  /** When a lookup last failed, if one has. */
  private val failed = Ref
    .in[SyncIO, IO, Option[FiniteDuration]](None)
    .unsafeRunSync()

  /**
    * Starts looking up the spellings that wait in `store` (see [[look]]), in
    * the background, unless it is already, or a lookup failed less than
    * [[rest]] ago. It is woken as passages are read, and goes on until none
    * wait, a lookup fails, or the browser stops the background, which it
    * resumes from at the next waking, as what waits is kept.
    */
  def wake
    (
      store: Store,
      storage: Browser.Storage,
      fetch: Lexicon.Fetch,
      frequencies: FrequencyService,
    )
    : IO[Unit] = (IO.monotonic, failed.get).flatMapN((now, last) =>
    running
      .getAndSet(true)
      .ifM(
        IO.unit,
        look(store, storage, fetch, frequencies)
          .guarantee(running.set(false))
          .start
          .void,
      )
      .whenA(last.forall(now - _ >= rest)),
  )

  /**
    * Looks up the spellings that wait in `store`, the first read first, one at
    * a time, every [[pace]], each counted toward the dictionary forms it is
    * found a form of, then no longer waiting, until none wait, the reader turns
    * Wiktionary off, or a lookup fails, whose spelling then waits behind the
    * others, lest one that always fails keep them all waiting.
    */
  private[codeswitch] def look
    (
      store: Store,
      storage: Browser.Storage,
      fetch: Lexicon.Fetch,
      frequencies: FrequencyService,
    )
    : IO[Unit] = storage
    .get(Lexicon.off)
    .flatMap(off =>
      store
        .waiting
        .flatMap {
          case first :: _ if !off(Dictionary.Wiktionary) =>
            resolve(first, store, fetch, frequencies)
              .attempt
              .flatMap {
                case Right(_) => IO.sleep(pace) >>
                    look(store, storage, fetch, frequencies)
                case Left(_) =>
                  (IO.monotonic, Timestamp.now).flatMapN((now, at) =>
                    failed.set(Some(now)) *> behind(first, at, store),
                  )
              }
          case _ => IO.unit
        },
    )

  /**
    * Keeps `waiting` as first read at `at`, behind the others, with what has
    * been added to it meanwhile, if it still waits.
    */
  private def behind
    (
      waiting: Pending,
      at: Timestamp,
      store: Store,
    )
    : IO[Unit] = Lexicon
    .lock
    .permit
    .surround(
      store
        .pending(List(waiting.key))
        .flatMap(kept =>
          store.pend(
            kept.flatten.map(_.copy(since = at)),
            Nil,
          ),
        ),
    )

  /**
    * Looks `waiting` up in Wiktionary, by `fetch`, begun in lower case first if
    * it began a sentence, then counts it toward the dictionary forms of the
    * lexemes it is a form of, if any, as often as it has been read by then, and
    * keeps it as waiting no longer.
    */
  private def resolve
    (
      waiting: Pending,
      store: Store,
      fetch: Lexicon.Fetch,
      frequencies: FrequencyService,
    )
    : IO[Unit] =
    val language = waiting.language
    val tried    = lowered(waiting.spelling.text)
      .filter(_ => waiting.opening)
      .map(Spelling(language, _))
      .toList :+ waiting.spelling
    tried
      .foldLeft(IO.pure(List.empty[String]))((found, spelling) =>
        found.flatMap(found =>
          if found.nonEmpty then IO.pure(found)
          else
            Lexicon.learn(language, spelling.text, store, fetch) *>
              store
                .homographs(List(spelling))
                .flatMap(lemmasOf(language, _, store))
                .map(_.getOrElse(spelling, Nil)),
        ),
      )
      .flatMap(forms =>
        Lexicon
          .lock
          .permit
          .surround(
            store
              .pending(List(waiting.key))
              .flatMap(kept =>
                val count = kept.head.fold(waiting.count)(_.count)
                frequencies
                  .count(
                    language,
                    forms.map(_ -> count / forms.size),
                  )
                  .whenA(forms.nonEmpty) *> store.pend(Nil, List(waiting.key)),
              ),
          ),
      )
