#[cfg(test)]
mod parse {
    use crate::parse::*;
    use crate::tree::Header::{Import, Using, Package};
    use crate::tree::Program;
    use crate::tree::Entry::HeaderEntry;

    #[test]
    fn works() {
        assert_eq!(1, 1)
    }

    #[test]
    fn parse_import() {
        let prog = parse("import streams");
        assert_eq!(prog, vec![HeaderEntry(Import("streams".into(), None))])
    }

    #[test]
    fn parse_import_dot() {
        let prog = parse("import codec.base64");
        assert_eq!(prog, vec![HeaderEntry(Import("codec.base64".into(), None))])
    }

    #[test]
    fn parse_import_as() {
        let prog = parse("import streams as s");
        assert_eq!(prog, vec![HeaderEntry(Import("streams".into(),
                                                 Some("s".into())))])
    }

    #[test]
    fn parse_import_dot_as() {
        let prog = parse("import codec.base64 as bbb");
        assert_eq!(prog, vec![HeaderEntry(Import("codec.base64".into(),
                                                 Some("bbb".into())))])
    }

    #[test]
    fn parse_using() {
        let prog = parse("using fuck");
        assert_eq!(prog, vec![HeaderEntry(Using("fuck".into()))])
    }

    #[test]
    fn parse_package() {
        let prog = parse("package fuck");
        assert_eq!(prog, vec![HeaderEntry(Package("fuck".into()))])
    }

    #[test]
    fn parse_mixed_headers() {
        let prog = parse(
            "package fuck\n\
            import junk as jk\n\
            import lolita as loli\n\
            import boynextdoor\n\
            using hololive\n\
            package homolive");
        assert_eq!(prog, vec![
            HeaderEntry(Package("fuck".into())),
            HeaderEntry(Import("junk".into(), Some("jk".into()))),
            HeaderEntry(Import("lolita".into(), Some("loli".into()))),
            HeaderEntry(Import("boynextdoor".into(), None)),
            HeaderEntry(Using("hololive".into())),
            HeaderEntry(Package("homolive".into())),
        ])
    }

    fn parse(input: &str) -> Program {
        CsParser::ast(input).ok().unwrap()
    }
}
