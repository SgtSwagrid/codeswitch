package com.alecdorrington.codeswitch

/** How Codeswitch looks, in the pages it translates and in its own popup. */
object Style:

  /** The colour of Codeswitch, as in its icon. */
  val colour = "#0f766e"

  /** The lighter colour of Codeswitch, for highlights. */
  val light = "#14b8a6"

  /**
    * The style of the translations in a page: highlighted, with the original
    * word above each on hovering over it. It overrides the page's own styles,
    * which might otherwise change how the original word is shown.
    */
  val css: String = s""".codeswitch {
       |  background: color-mix(in srgb, $light 22%, transparent) !important;
       |  box-shadow: inset 0 -2px 0 $light !important;
       |  border-radius: 3px !important;
       |  cursor: help !important;
       |  position: relative !important;
       |}
       |.codeswitch:hover::after {
       |  all: initial;
       |  content: attr(data-codeswitch);
       |  position: absolute;
       |  left: 50%;
       |  bottom: calc(100% + 6px);
       |  transform: translateX(-50%);
       |  padding: 3px 8px;
       |  border-radius: 6px;
       |  background: $colour;
       |  color: #fff;
       |  font: 600 13px/1.4 system-ui, sans-serif;
       |  white-space: nowrap;
       |  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
       |  pointer-events: none;
       |  z-index: 2147483647;
       |}
       |""".stripMargin

  /** The style of the popup. */
  val popup: String = s"""body {
       |  width: 300px;
       |  margin: 0;
       |  padding: 14px 16px;
       |  font: 14px/1.4 system-ui, sans-serif;
       |  color: #1f2937;
       |  background: #fff;
       |}
       |header { display: flex; align-items: center; gap: 10px; margin-bottom: 12px; }
       |header img { width: 28px; height: 28px; }
       |h1 { margin: 0; font-size: 18px; color: $colour; }
       |label { display: block; margin-bottom: 10px; }
       |label span { display: block; margin-bottom: 4px; font-weight: 600; }
       |select, input {
       |  box-sizing: border-box;
       |  width: 100%;
       |  padding: 6px 8px;
       |  font: inherit;
       |  border: 1px solid #cbd5e1;
       |  border-radius: 6px;
       |}
       |select:focus, input:focus { outline: 2px solid $light; border-color: $light; }
       |.note { margin: 0; font-size: 12px; color: #64748b; }
       |.problem { margin: 10px 0 0; color: #b91c1c; }
       |.problem:empty { display: none; }
       |@media (prefers-color-scheme: dark) {
       |  body { color: #e5e7eb; background: #1f2937; }
       |  h1 { color: $light; }
       |  select, input { color: inherit; background: #111827; border-color: #4b5563; }
       |  .note { color: #9ca3af; }
       |  .problem { color: #fca5a5; }
       |}
       |""".stripMargin
