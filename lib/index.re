open BetterErrorsTypes;

open Helpers;

/* the compiler output might point to an error that spans across many lines;
   however, instead of indicating from (startRow, startColumn) to (endRow,
   endColumn), it'll indicate (startRow, startColumn, endColumn) where endColumn
   might belong to a different row! We normalize and find the row here */
/* the compiler line number is 1-indexed, and col number is 0-indexed but the
   endColumn for an error goes past the last "expected" endColumn, e.g. if it's
   `typ a = string`
   instead of saying it's from 0 to 2, it shows as 0 to 3. This is also kinda
   expected, since you get easy column count through 3 - 0 */
/* we'll use 0-indexed. It's a reporter (printer)'s job to normalize to
   1-indexed if it desires so */
let normalizeCompilerLineColsToRange = (~fileLines, ~lineRaw, ~col1Raw, ~col2Raw) => {
  /* accept strings to constraint usage to parse directly from raw data */
  let line = int_of_string(lineRaw);
  let fileLength = List.length(fileLines);
  let isOCamlBeingBadAndPointingToALineBeyondFileLength = line > fileLength;
  let (col1, col2) =
    if (isOCamlBeingBadAndPointingToALineBeyondFileLength) {
      let lastDamnReachableSpotInTheFile = String.length(List.nth(fileLines, fileLength - 1));
      (lastDamnReachableSpotInTheFile - 1, lastDamnReachableSpotInTheFile)
    } else {
      switch (col1Raw, col2Raw) {
      | (Some(a), Some(b)) => (int_of_string(a), int_of_string(b))
      /* some error msgs don't have column numbers; we normal them to 0 here */
      | _ => (0, 0)
      }
    };
  let startRow =
    if (isOCamlBeingBadAndPointingToALineBeyondFileLength) {
      fileLength - 1
    } else {
      line - 1
    };
  let currentLine = List.nth(fileLines, startRow);
  let numberOfCharsBetweenStartAndEndColumn = col2 - col1;
  let numberOfCharsLeftToCoverOnStartingRow =
    /* +1 bc ocaml looooves to count new line as a char below when the error
       spans multiple lines*/
    String.length(currentLine) - col1 + 1;
  if (numberOfCharsBetweenStartAndEndColumn <= numberOfCharsLeftToCoverOnStartingRow) {
    ((startRow, col1), (startRow, col2))
  } else {
    let howManyCharsLeftToCoverOnSubsequentLines =
      ref(numberOfCharsBetweenStartAndEndColumn - numberOfCharsLeftToCoverOnStartingRow);
    let suddenlyFunctionalProgrammingOutOfNowhere =
      fileLines
      |> Helpers.listDrop(startRow + 1)
      |> List.map(String.length)
      |> Helpers.listTakeWhile(
           (numberOfCharsOnThisLine) =>
             if (howManyCharsLeftToCoverOnSubsequentLines^ > numberOfCharsOnThisLine) {
               howManyCharsLeftToCoverOnSubsequentLines :=
                 howManyCharsLeftToCoverOnSubsequentLines^ - numberOfCharsOnThisLine - 1;
               true
             } else {
               false
             }
         );
    let howManyMoreRowsCoveredSinceStartRow =
      1 + List.length(suddenlyFunctionalProgrammingOutOfNowhere);
    (
      (startRow, col1),
      (startRow + howManyMoreRowsCoveredSinceStartRow, howManyCharsLeftToCoverOnSubsequentLines^)
    )
  }
};

/* has the side-effect of reading the file */
let extractFromFileMatch = (fileMatch) =>
  Re_pcre.(
    switch fileMatch {
    | [Delim(_), Group(_, filePath), Group(_, lineNum), col1, col2, Text(body)] =>
      let cachedContent = Helpers.fileLinesOfExn(filePath);
      /* sometimes there's only line, but no characters */
      let (col1Raw, col2Raw) =
        switch (col1, col2) {
        | (Group(_, c1), Group(_, c2)) =>
          /* bug: https://github.com/mmottl/pcre-ocaml/issues/5 */
          if (String.trim(c1) == "" || String.trim(c2) == "") {
            (None, None)
          } else {
            (Some(c1), Some(c2))
          }
        | _ => (None, None)
        };
      (
        filePath,
        cachedContent,
        normalizeCompilerLineColsToRange(
          ~fileLines=cachedContent,
          ~lineRaw=lineNum,
          ~col1Raw,
          ~col2Raw
        ),
        /* important, otherwise leaves random blank lines that defies some of
           our regex logic, maybe */
        String.trim(body)
      )
    | _ => raise(invalid_arg("Couldn't extract error"))
    }
  );

/* debug helper */
let printFullSplitResult =
  List.iteri(
    (i, x) => {
      print_int(i);
      print_endline("");
      Re_pcre.(
        switch x {
        | Delim(a) => print_endline("Delim " ++ a)
        | Group(_, a) => print_endline("Group " ++ a)
        | Text(a) => print_endline("Text " ++ a)
        | NoGroup => print_endline("NoGroup")
        }
      )
    }
  );

let fileR =
  Re_pcre.regexp(
    ~flags=[Re_pcre.(`MULTILINE)],
    {|^File "([\s\S]+?)", line (\d+)(?:, characters (\d+)-(\d+))?:$|}
  );

let hasErrorOrWarningR = Re_pcre.regexp(~flags=[Re_pcre.(`MULTILINE)], {|^(Error|Warning \d+): |});

let hasIndentationR = Re_pcre.regexp(~flags=[Re_pcre.(`MULTILINE)], {|^       +|});

/* TODO: make the below work. the "Here is an example..." is followed by even more lines of hints */
/* let hasHintRStr = {|^(Hint: Did you mean |Here is an example of a value that is not matched:)|} */
/* let hasHintRStr = {|^(Here is an example of a value that is not matched:|Hint: Did you mean )|} */
let hasHintRStr = {|^Hint: Did you mean |};

let argCannotBeAppliedWithLabelRStr = {|^This argument cannot be applied with label|};

let hasHintR = Re_pcre.regexp(~flags=[Re_pcre.(`MULTILINE)], hasHintRStr);

let argCannotBeAppliedWithLabelR =
  Re_pcre.regexp(~flags=[Re_pcre.(`MULTILINE)], argCannotBeAppliedWithLabelRStr);

let notVisibleInCurrentScopeStr = {|^not visible in the current scope|};

let notVisibleInCurrentScopeR =
  Re_pcre.regexp(~flags=[Re_pcre.(`MULTILINE)], notVisibleInCurrentScopeStr);

let theyWillNotBeSelectedStr = {|^They will not be selected|};

let theyWillNotBeSelectedR =
  Re_pcre.regexp(~flags=[Re_pcre.(`MULTILINE)], theyWillNotBeSelectedStr);

let parse = (~customLogOutputProcessors, ~customErrorParsers, err) => {
  /* we know whatever err is, it starts with "File: ..." because that's how `parse`
     is used */
  let err = String.trim(err);
  try (
    switch (Re_pcre.full_split(~rex=fileR, err)) {
    | [Re_pcre.Delim(_), Group(_, filePath), Group(_, lineNum), col1, col2, Text(body)] =>
      /* important, otherwise leaves random blank lines that defies some of
         our regex logic, maybe */
      let body = String.trim(body);
      let errorCapture = get_match_maybe({|^Error: ([\s\S]+)|}, body);
      switch (ParseError.specialParserThatChecksWhetherFileEvenExists(filePath, errorCapture)) {
      | Some(err) => err
      | None =>
        let cachedContent = Helpers.fileLinesOfExn(filePath);
        /* sometimes there's only line, but no characters */
        let (col1Raw, col2Raw) =
          switch (col1, col2) {
          | (Group(_, c1), Group(_, c2)) =>
            /* bug: https://github.com/mmottl/pcre-ocaml/issues/5 */
            if (String.trim(c1) == "" || String.trim(c2) == "") {
              raise(Invalid_argument("HUHUHUH"))
            } else {
              (Some(c1), Some(c2))
            }
          | _ => (None, None)
          };
        let range =
          normalizeCompilerLineColsToRange(
            ~fileLines=cachedContent,
            ~lineRaw=lineNum,
            ~col1Raw,
            ~col2Raw
          );
        let warningCapture =
          switch (execMaybe({|^Warning (\d+): ([\s\S]+)|}, body)) {
          | None => (None, None)
          | Some(capture) => (getSubstringMaybe(capture, 1), getSubstringMaybe(capture, 2))
          };
        switch (errorCapture, warningCapture) {
        | (Some(errorBody), (None, None)) =>
          ErrorContent({
            filePath,
            cachedContent,
            range,
            parsedContent:
              ParseError.parse(~customErrorParsers, ~errorBody, ~cachedContent, ~range)
          })
        | (None, (Some(code), Some(warningBody))) =>
          let code = int_of_string(code);
          Warning({
            filePath,
            cachedContent,
            range,
            parsedContent: {
              code,
              warningType: ParseWarning.parse(code, warningBody, filePath, cachedContent, range)
            }
          })
        | _ => raise(Invalid_argument(err))
        }
      }
    /* not an error, not a warning. False alarm? */
    | _ => Unparsable
    }
  ) {
  | _ => Unparsable
  }
};

let line_stream_of_channel = (channel) =>
  Stream.from(
    (_) =>
      try (Some(input_line(channel))) {
      | End_of_file => None
      }
  );

/* entry point, for convenience purposes for now. Theoretically the parser and
      the reporters are decoupled.
      What about errors of the form:

   */
let revBufferToStr = (revBuffer) => String.concat("\n", List.rev(revBuffer));

let parseFromStdin = (~refmttypePath, ~customLogOutputProcessors, ~customErrorParsers) => {
  let reverseErrBuffer = {contents: []};
  let prettyPrintParsedResult = TerminalReporter.prettyPrintParsedResult(~refmttypePath);
  let forEachLine = (line) =>
    switch (
      reverseErrBuffer.contents,
      Re_pcre.pmatch(~rex=fileR, line),
      Re_pcre.pmatch(~rex=hasErrorOrWarningR, line),
      Re_pcre.pmatch(~rex=hasIndentationR, line)
    ) {
    | ([], false, false, false) =>
      /* no error, just stream on the line */
      print_endline(TerminalReporter.processLogOutput(~customLogOutputProcessors, line))
    | ([], true, _, _)
    | ([], _, true, _)
    | ([], _, _, true) =>
      /* the beginning of a new error! */
      reverseErrBuffer.contents = [line]
    /* don't parse it yet. Maybe the error's continuing on the next line */
    | (_, true, _, _) =>
      /* we have a file match, AND the current reverseErrBuffer isn't empty? We'll
         just assume here that this is also the beginning of a new error, unless
         a single error might span many (non-indented, god forbid) fileNames.
         Print out the current (previous) error and keep accumulating */
      let bufferText = revBufferToStr(reverseErrBuffer.contents);
      parse(~customLogOutputProcessors, ~customErrorParsers, bufferText)
      |> prettyPrintParsedResult(~originalRevLines=reverseErrBuffer.contents)
      |> revBufferToStr
      |> print_endline;
      reverseErrBuffer.contents = [line]
    /* buffer not empty, and we're seeing an error/indentation line. This is
       the continuation of a currently streaming error/warning */
    | (_, _, _, true)
    | (_, _, true, _) => reverseErrBuffer.contents = [line, ...reverseErrBuffer.contents]
    | (_, false, false, false) =>
      /* woah this case was previously forgotten but caught by the
             compiler. Man I don't ever wanna write an if-else anymore
             buffer not empty, and no indentation and not an error/file
             line? This means the previous error might have ended. We say
             "might" because some errors provide non-indented messages...
             here's one such case (hasHintR). And here's another:
             Error: The function applied to this argument has type
                      customLogOutputProcessors:(string -> string) list ->
                      customErrorParsers:(string * string list) list -> unit
             This argument cannot be applied with label ~raiseExceptionDuringParse
         */
      if (Re_pcre.pmatch(~rex=hasHintR, line)
          || Re_pcre.pmatch(~rex=argCannotBeAppliedWithLabelR, line)
          || Re_pcre.pmatch(~rex=notVisibleInCurrentScopeR, line)
          || Re_pcre.pmatch(~rex=theyWillNotBeSelectedR, line)) {
        reverseErrBuffer.contents =
          [line, ...reverseErrBuffer.contents]
          /* let bufferText = revBufferToStr(reverseErrBuffer.contents);
           * parse(~customLogOutputProcessors, ~customErrorParsers, bufferText)
           * |> prettyPrintParsedResult(~originalRevLines=reverseErrBuffer.contents)
           * |> revBufferToStr
           * |> print_endline;
           * reverseErrBuffer.contents = []
           */
      } else {
        let bufferText = revBufferToStr(reverseErrBuffer.contents);
        parse(~customLogOutputProcessors, ~customErrorParsers, bufferText)
        |> prettyPrintParsedResult(~originalRevLines=reverseErrBuffer.contents)
        |> revBufferToStr
        |> print_endline;
        reverseErrBuffer.contents = [line]
      }
    };
  try {
    line_stream_of_channel(stdin) |> Stream.iter(forEachLine);
    /* might have accumulated a few more lines */
    if (reverseErrBuffer.contents !== []) {
      let bufferText = revBufferToStr(reverseErrBuffer.contents);
      parse(~customLogOutputProcessors, ~customErrorParsers, bufferText)
      |> prettyPrintParsedResult(~originalRevLines=reverseErrBuffer.contents)
      |> revBufferToStr
      |> print_endline
    };
    close_in(stdin)
  } {
  | e =>
    close_in(stdin);
    raise(e)
  }
};
