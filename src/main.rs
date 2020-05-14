use clipboard::{ClipboardContext, ClipboardProvider};
use device_query::{DeviceQuery, DeviceState, Keycode};
use enigo::{Enigo, Key, KeyboardControllable};
use regex::Regex;
use std::{error::Error, mem, thread, time::Duration};

//TODO:
// - Support deriving matrices from equations.
//
// - Long term: Do a nicer script with OCR and sound notifications.

const DEFAULT_TRIGGER_KEY: Keycode = Keycode::F8;
const KEYCODES: &[Keycode] = {
    use device_query::Keycode::*;
    &[
        Key0,
        Key1,
        Key2,
        Key3,
        Key4,
        Key5,
        Key6,
        Key7,
        Key8,
        Key9,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        U,
        V,
        W,
        X,
        Y,
        Z,
        F1,
        F2,
        F3,
        F4,
        F5,
        F6,
        F7,
        F8,
        F9,
        F10,
        F11,
        F12,
        Escape,
        Space,
        LControl,
        RControl,
        LShift,
        RShift,
        LAlt,
        RAlt,
        Meta,
        Enter,
        Up,
        Down,
        Left,
        Right,
        Backspace,
        CapsLock,
        Tab,
        Home,
        End,
        PageUp,
        PageDown,
        Insert,
        Delete,
        Grave,
        Minus,
        Equal,
        LeftBracket,
        RightBracket,
        BackSlash,
        Semicolon,
        Apostrophe,
        Comma,
        Dot,
        Slash,
    ]
};
const TUTORIAL: &str = r#"
labwolfram: Convierte matrices del lab a wolfram y viceversa
 - Martin Andrighetti, UC 2020

Manten esta ventana abierta en segundo plano hasta que ya no necesites convertir
matrices. El programa no es infalible, revisa siempre!

Para pasar matrices del LAB -> WOLFRAM:
    Haz click derecho en la matriz, y selecciona:
    "Configuracion matematica > Renderizador matematico > Plain Source"
    Selecciona el codigo que aparece y copialo con Ctrl+C.
    Anda a wolfram.
    Apreta <trigger key> para pegar la matriz en formato wolfram.
    Nota: Puedes volver a ver las matrices del lab seleccionando la opcion de
    "Configuracion matematica > Renderizador matematico > HTML-CSS".

Para pasar matrices de WOLFRAM -> LAB:
    Copia el codigo de la matriz de wolfram con Ctrl+C.
    Anda al primer casillero de la matriz en el lab.
    Apreta <trigger key> para ingresar los elementos de la matriz
    automaticamente.
"#;

#[derive(Default, Debug)]
struct Matrix {
    name: String,
    rows: Vec<Vec<String>>,
}
impl Matrix {
    fn check(&self) -> Result<(), Box<dyn Error>> {
        let mut columns = None;
        for row in self.rows.iter() {
            let cols = columns.get_or_insert(row.len());
            if row.len() != *cols {
                return Err("jagged matrix".into());
            }
        }
        match (columns, self.rows.len()) {
            (None, _) | (Some(0), _) => Err("empty matrix".into()),
            (Some(1), 1) => Err("1x1 matrix not considered a matrix".into()),
            _ => Ok(()),
        }
    }
    fn rate(&self) -> f32 {
        let mut score = 0.0;
        for row in self.rows.iter() {
            for elem in row.iter() {
                if !elem.is_empty() {
                    score += 1.0;
                }
            }
        }
        score
    }
    fn print(&self) {
        let max_len = self
            .rows
            .iter()
            .flat_map(|row| row.iter())
            .map(|elem| elem.len())
            .max()
            .unwrap_or(0);
        for row in self.rows.iter() {
            for elem in row.iter() {
                let pad = max_len - elem.len();
                let pad_left = pad / 2 + pad % 2;
                let pad_right = pad / 2;
                for _ in 0..pad_left {
                    eprint!(" ");
                }
                eprint!("  {}", elem);
                for _ in 0..pad_right {
                    eprint!(" ");
                }
            }
            eprintln!();
        }
    }

    fn read_lab<'a>(ctx: &mut Context, text: &mut &'a str) -> Result<Matrix, Box<dyn Error>> {
        if ctx.debug {
            eprintln!("parsing as lab matrix");
        }
        #[derive(Debug, PartialEq, Eq)]
        enum Closing {
            Any,
            Char(char),
            Str(String),
        }
        let (open, top_elem) = {
            let a = Regex::new(r#"\\begin\{array\}\s*\{c+\}"#)
                .unwrap()
                .find(text)
                .map(|mat| mat.end());
            let b = Regex::new(r#"\{c+\}()[^\&]+\&"#)
                .unwrap()
                .captures(text)
                .map(|mat| mat.get(1).unwrap().start());
            let c = Regex::new(r#"[^\}\&]+\&"#)
                .unwrap()
                .find(text)
                .map(|mat| mat.start());
            enum Marks {
                Begin,
                Columns,
                Fallback,
            }
            let mut marks = [(Marks::Fallback, c), (Marks::Columns, b), (Marks::Begin, a)];
            marks.sort_by_key(|(m, idx)| {
                if let Marks::Fallback = m {
                    Some(isize::min_value())
                } else {
                    idx.map(|idx| -(idx as isize))
                }
            });
            let (mark, idx) = marks.last().unwrap();
            let idx = idx.ok_or("no starting mark found")?;
            let top_elem = match mark {
                Marks::Begin => {
                    if ctx.debug {
                        eprintln!("using \\begin mark at {}", idx);
                    }
                    Closing::Str("\\end{array}".to_string())
                }
                Marks::Columns => {
                    if ctx.debug {
                        eprintln!("using {{c+}} mark at {}", idx);
                    }
                    Closing::Any
                }
                Marks::Fallback => {
                    if ctx.debug {
                        eprintln!("using fallback }} mark at {}", idx);
                    }
                    Closing::Any
                }
            };
            (idx, top_elem)
        };
        let mut matrix = Matrix::default();
        matrix.name = {
            let re = Regex::new(r#"(\w+)\s*="#).unwrap();
            let mut last_find = None;
            let mut idx = 0;
            while let Some(mat) = re.captures(&text[idx..open]) {
                idx += mat.get(0).unwrap().end();
                last_find = Some(mat.get(1).unwrap().as_str());
            }
            if ctx.debug {
                if let Some(last) = last_find {
                    eprintln!("last name found is \"{}\"", last);
                }
            }

            last_find.unwrap_or("").to_string()
        };
        let lab = &text[open..];
        *text = lab;
        let mut idx = 0;
        let mut last_idx = 0;
        let mut stack = vec![top_elem];
        let mut cur_row = Vec::new();
        let next_control = Regex::new(r#"(?x)
            (?P<close> \} | \] | \) | (?:\\end\{[^}]*\}) ) |
            (?P<colsep> \&) | (?P<rowsep> \\\\) |
            (?P<opencurly> \{) | (?P<opensquare> \[) | (?P<openround> \() | (?:\\begin\{(?P<openlatex> [^\}]*)\})
            "#).unwrap();
        while let Some(stack_top) = stack.last() {
            if let Some(mat) = next_control.captures(&lab[idx..]) {
                let base_idx = idx;
                idx += mat.get(0).unwrap().start();
                *text = &lab[idx..];
                if let Some(close) = mat.name("close") {
                    let is_equal = match stack_top {
                        Closing::Any => true,
                        Closing::Char(ch) => {
                            let mut buf = [0; 4];
                            close.as_str() == ch.encode_utf8(&mut buf)
                        }
                        Closing::Str(str) => close.as_str() == *str,
                    };
                    if !is_equal {
                        return Err(format!(
                            "expected closing delimiter {:?}, got {}",
                            stack_top,
                            close.as_str()
                        )
                        .into());
                    }
                    stack.pop();
                    if stack.is_empty() {
                        break;
                    }
                } else if let Some(colsep) = mat.name("colsep") {
                    // &
                    if stack.len() == 1 {
                        let num = lab[last_idx..base_idx + colsep.start()].trim();
                        if ctx.debug {
                            eprintln!("pushing number {}", num);
                        }
                        cur_row.push(num.to_string());
                        last_idx = base_idx + colsep.end();
                    }
                } else if let Some(rowsep) = mat.name("rowsep") {
                    // \\
                    if stack.len() == 1 {
                        let num = lab[last_idx..base_idx + rowsep.start()].trim();
                        if ctx.debug {
                            eprintln!("pushing number {} and newrow", num);
                        }
                        cur_row.push(num.to_string());
                        matrix.rows.push(mem::replace(&mut cur_row, Vec::new()));
                        last_idx = base_idx + rowsep.end();
                    }
                } else if let Some(_) = mat.name("opencurly") {
                    stack.push(Closing::Char('}'));
                } else if let Some(_) = mat.name("opensquare") {
                    stack.push(Closing::Char(']'));
                } else if let Some(_) = mat.name("openround") {
                    stack.push(Closing::Char(')'));
                } else if let Some(id) = mat.name("openlatex") {
                    stack.push(Closing::Str(format!("\\end{{{}}}", id.as_str())));
                }
                idx += mat.get(0).unwrap().as_str().len();
                *text = &lab[idx..];
            } else {
                idx = lab.len();
                *text = &lab[idx..];
                break;
            }
        }
        if matrix
            .rows
            .last()
            .map(|last| last.len() == cur_row.len() + 1)
            .unwrap_or(false)
        {
            if ctx.debug {
                eprintln!("missing just one final num");
            }
            let final_num = lab[last_idx..idx].trim();
            if !final_num.is_empty() {
                if ctx.debug {
                    eprintln!(
                        "  pushing one final num/row without delimiter: \"{}\"",
                        final_num
                    );
                }
                cur_row.push(final_num.to_string());
                matrix.rows.push(cur_row);
            }
        }
        matrix.check()?;
        Ok(matrix)
        /*
        let open = ctx.begin_array.captures(lab).ok_or("no \\begin{array}")?;
        let column_count = open[1].len();
        let contents = &lab[open.get(0).unwrap().end()..];
        let close = ctx.end_array.find(contents).ok_or("no \\end{array}")?;
        let contents = &contents[..close.start()];
        let mut stack = Vec::new();
        let mut last_char = ' ';
        let mut num_start = 0;
        let mut rows = Vec::new();
        let mut cur_row = Vec::new();
        const STACK_CHARS: &[(char, char)] = &[('{', '}'), ('[', ']'), ('(', ')')];
        for (i, ch) in contents.char_indices() {
            if let Some(&(_open, close)) = STACK_CHARS.iter().find(|(open, _close)| open == &ch) {
                stack.push(close);
            } else {
                if let Some(&close) = stack.last() {
                    if ch == close {
                        stack.pop();
                    }
                } else {
                    match ch {
                        '&' => {
                            //Next column
                            let num = &contents[num_start..i];
                            num_start = i + 1;
                            cur_row.push(num);
                        }
                        '\\' if last_char == '\\' => {
                            //Next row
                            let num = &contents[num_start..i - 1];
                            num_start = i + 1;
                            cur_row.push(num);
                            rows.push(cur_row);
                            cur_row = Vec::new();
                        }
                        _ => {}
                    }
                }
            }
            last_char = ch;
        }

        Ok(())*/
    }

    fn read_wolfram<'a>(ctx: &mut Context, text: &mut &'a str) -> Result<Matrix, Box<dyn Error>> {
        if ctx.debug {
            eprintln!("parsing as wolfram matrix");
        }
        let open = text.find('{').ok_or("no opening { found")?;
        let wolfram = &text[open + 1..];
        *text = wolfram;
        let mut stack = vec!['}'];
        let mut finished_group = false;
        let mut num_start = 0;
        let mut matrix = Matrix::default();
        let mut cur_row = Vec::new();
        const STACK_CHARS: &[(char, char)] = &[('{', '}'), ('[', ']'), ('(', ')')];
        for (i, ch) in wolfram.char_indices() {
            *text = &wolfram[i + ch.len_utf8()..];
            if let Some(&(_open, close)) = STACK_CHARS.iter().find(|(open, _close)| open == &ch) {
                if ctx.debug {
                    eprintln!("opening with {} at {}", ch, i);
                }
                if finished_group {
                    //Cannot open a group just as the last one finished
                    return Err("cannot open group after row close".into());
                }
                stack.push(close);
                match stack.len() {
                    2 => {
                        //Opened a row
                        if ctx.debug {
                            eprintln!("  opened a row");
                        }
                        if close != '}' {
                            return Err("row container must be {}".into());
                        }
                        num_start = i + 1;
                    }
                    _ => {}
                }
            } else {
                if let Some(&close) = stack.last() {
                    if ch == close {
                        if ctx.debug {
                            eprintln!("closing with {} at {}", close, i);
                        }
                        stack.pop();
                        match stack.len() + 1 {
                            1 => {
                                //Closed the matrix
                                if ctx.debug {
                                    eprintln!("  closed the matrix itself")
                                };
                                break;
                            }
                            2 => {
                                //Closed a row
                                let num = wolfram[num_start..i].trim();
                                if ctx.debug {
                                    eprintln!("  closed a row with num \"{}\"", num);
                                }
                                if !num.is_empty() {
                                    cur_row.push(num.to_string());
                                }
                                matrix.rows.push(cur_row);
                                cur_row = vec![];
                                //Just after finishing a row the only acceptable characters are
                                //whitespace, delimiters and group close
                                finished_group = true;
                            }
                            _ => {}
                        }
                    } else if ch == ',' {
                        //Comma clears `finished_group`, and new groups can be opened
                        finished_group = false;
                        if ctx.debug {
                            eprintln!("separator at {}", i);
                        }
                        match stack.len() {
                            1 => {
                                //Separator between rows
                                if ctx.debug {
                                    eprintln!("  separated rows");
                                }
                            }
                            2 => {
                                //Separator between columns
                                let num = wolfram[num_start..i].trim();
                                if ctx.debug {
                                    eprintln!("  separated columns with num \"{}\"", num);
                                }
                                cur_row.push(num.to_string());
                                num_start = i + 1;
                            }
                            _ => {}
                        }
                    } else {
                        if finished_group && !ch.is_whitespace() {
                            return Err(format!(
                                "expected comma or group close at {}, found '{}'",
                                i, ch
                            )
                            .into());
                        }
                    }
                }
            }
        }
        matrix.check()?;
        Ok(matrix)
    }

    fn write_lab(&self, ctx: &mut Context) {
        ctx.send.key_click(Key::Space);
        ctx.send.key_click(Key::Backspace);
        let mut first = true;
        for row in self.rows.iter() {
            for elem in row.iter() {
                if !first {
                    ctx.send.key_click(Key::Tab);
                } else {
                    first = false;
                }
                ctx.send.key_sequence(elem);
            }
        }
    }

    fn write_wolfram(&self, ctx: &mut Context) {
        let mut wolfram = String::new();
        if !self.name.is_empty() {
            wolfram.push_str(&self.name);
            wolfram.push_str(" = ");
        }
        wolfram.push('{');
        for (i, row) in self.rows.iter().enumerate() {
            if i > 0 {
                wolfram.push_str(", ");
            }
            wolfram.push('{');
            for (j, num) in row.iter().enumerate() {
                if j > 0 {
                    wolfram.push_str(", ");
                }
                wolfram.push_str(num.trim());
            }
            wolfram.push('}');
        }
        wolfram.push('}');
        ctx.send.key_click(Key::Space);
        ctx.send.key_click(Key::Backspace);
        ctx.send.key_sequence(&wolfram);
    }

    fn read_many(
        ctx: &mut Context,
        mut text: &str,
        mut read: impl FnMut(&mut Context, &mut &str) -> Result<Matrix, Box<dyn Error>>,
    ) -> Result<Vec<Matrix>, Box<dyn Error>> {
        const LETTERS: &str = "ABCDE";
        fn gen_name(idx: usize, out: &mut String) {
            out.clear();
            let letter = idx % LETTERS.len();
            let num = idx / LETTERS.len();
            out.push_str(&LETTERS[letter..letter + 1]);
            if num > 0 {
                use std::fmt::Write;
                let _ = write!(out, "{}", num - 1);
            }
        }
        let mut matrices = Vec::new();
        let mut errors = Vec::new();
        loop {
            let old_text = text;
            match read(ctx, &mut text) {
                Ok(mat) => {
                    matrices.push(mat);
                }
                Err(err) => {
                    errors.push(err);
                    if text == old_text {
                        break;
                    }
                }
            }
        }
        if matrices.is_empty() {
            let error = errors
                .iter()
                .map(|err| format!("{}", err))
                .collect::<Vec<String>>()
                .join(", ");
            Err(error.into())
        } else {
            if matrices.len() > 1 {
                for i in 0..matrices.len() {
                    if matrices[i].name.is_empty() {
                        //Fill in the name for this matrix
                        let mut name = String::new();
                        let mut nameidx = 0;
                        loop {
                            gen_name(nameidx, &mut name);
                            if !matrices.iter().any(|mat| mat.name == name) {
                                //This name is free
                                break;
                            }
                            nameidx += 1;
                        }
                        matrices[i].name = name;
                    }
                }
            }
            Ok(matrices)
        }
    }
}

struct Context {
    send: Enigo,
    recv: DeviceState,
    clip: ClipboardContext,
    debug: bool,
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.len() > 0 {
        eprintln!("arguments: labwolfram <trigger keycode> [--debug]");
    }
    let debug = args.iter().any(|arg| arg == "--debug");
    let trigger_key = args
        .get(0)
        .and_then(|keyname| {
            let keyname = keyname.to_lowercase();
            for key in KEYCODES {
                if format!("{:?}", key).to_lowercase() == keyname {
                    return Some(key);
                }
            }
            eprintln!("unknown keycode \"{}\", using default", keyname);
            eprintln!("keycode list: ");
            for key in KEYCODES {
                eprint!("{:?}, ", key);
            }
            eprintln!();
            None
        })
        .unwrap_or(&DEFAULT_TRIGGER_KEY);
    let mut ctx = Context {
        send: Enigo::new(),
        recv: DeviceState::new(),
        clip: ClipboardContext::new().unwrap(),
        debug,
    };
    let mut was_pressed = false;
    if ctx.debug {
        eprintln!("debug output enabled");
    }
    eprintln!(
        "{}",
        TUTORIAL.replace("<trigger key>", &format!("{:?}", trigger_key))
    );
    loop {
        let keys = ctx.recv.get_keys();
        let pressed = keys.contains(trigger_key);
        if !pressed && was_pressed {
            match ctx.clip.get_contents() {
                Ok(text) => {
                    if ctx.debug {
                        eprintln!("received clipboard text: r#\"{}\"#", text);
                    }
                    macro_rules! format_op {
                        (@read $name:expr, $read:expr) => {{
                            match Matrix::read_many(&mut ctx, &text, $read) {
                                Ok(matrices) => (
                                    None,
                                    matrices.iter().map(Matrix::rate).sum::<f32>(),
                                    matrices,
                                ),
                                Err(err) => {
                                    if ctx.debug {
                                        eprintln!("{} error: {:?}", $name, err);
                                    }
                                    (Some(err), -1.0, Vec::new())
                                }
                            }
                        }};
                        (@printmatrices $name:expr, $mat:expr) => {{
                            for mat in $mat.iter() {
                                eprintln!(
                                    "{} matrix ({}x{}):",
                                    $name,
                                    mat.rows.len(),
                                    mat.rows.first().map(|r| r.len()).unwrap_or(0)
                                );
                                mat.print();
                            }
                        }};
                        (@writematrices $fromname:expr, $toname:expr, $from:expr, $write:ident) => {{
                            if $from.len() == 1 {
                                eprintln!(
                                    "Matriz de {} convertida a matriz de {}",
                                    $fromname, $toname
                                );
                            } else {
                                eprintln!(
                                    "{} matrices de {} convertidas a matrices de {}",
                                    $from.len(),
                                    $fromname,
                                    $toname,
                                );
                            }
                        }};
                    };
                    let (laberr, labscore, lab) = format_op!(@read "lab", Matrix::read_lab);
                    let (wolerr, wolscore, wol) = format_op!(@read "wolfram", Matrix::read_wolfram);
                    if laberr.is_some() && wolerr.is_some() {
                        eprintln!(
                            "No se reconoce el texto copiado como una matriz de lab o de wolfram"
                        );
                        eprintln!("Texto copiado: \"{}\"", text);
                    } else {
                        if ctx.debug {
                            format_op!(@printmatrices "lab", lab);
                            format_op!(@printmatrices "wolfram", wol);
                        }
                        if labscore >= wolscore {
                            let mut first = true;
                            for mat in lab.iter() {
                                if first {
                                    first = false;
                                } else {
                                    ctx.send.key_click(Key::Return);
                                }
                                mat.write_wolfram(&mut ctx);
                            }
                            format_op!(@writematrices "lab", "wolfram", lab, write_wolfram);
                        } else {
                            for mat in wol.iter().take(1) {
                                mat.write_lab(&mut ctx);
                            }
                            format_op!(@writematrices "wolfram", "lab", wol, write_lab);
                        }
                    }
                }
                Err(err) => {
                    eprintln!("Copia una matriz del lab o de wolfram primero");
                    if ctx.debug {
                        eprintln!("clipboard error: {:?}", err);
                    }
                }
            }
            eprintln!();
        }
        was_pressed = pressed;
        thread::sleep(Duration::from_millis(50));
    }
}

// A = \left[\begin{array}{ccc} -2 & 1 & -1 \\ 5 & -3 & 2 \\ 9 & 10 & -4 \\ \end{array}\right]
// B = \left[\begin{array}{ccc} -2 & 1 & -1 \\ 5 & -3 & 2 \\ 9 & 10 & -4 \end{array}\right]
// {{-2, 1, -1,2}, {5, -3, 2,1}, {9, 3,{10, a, b[]}, -4}}
