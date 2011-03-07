// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas;

import java.util.*;
import java.util.regex.Pattern;
import java.util.zip.*;
import java.io.*;

public class Wizard {
    public static final String BASE = "base";
    public static final String DESCRIPTION = "description";
    public static final String ID = "id";
    public static final String EXT = "ext";
    public static final String CLASS = "class";
    public static final String PACKAGE = "package";

    public static class BadParam extends Exception {
        public ParamInfo param;
        public String value;

        BadParam(ParamInfo param, String value) {
            super(param.description + " ("
                    + value + ") does not match pattern "
                    + param.regexp);

            this.param = param;
            this.value = value;
        }
    }

    public String target;
    public boolean copyDotFiles = true;

    //hopefully larger than any file
    private byte[] buf = new byte[32768];
    private Map params = new HashMap();

    private static final ParamInfo[] PARAM_INFO = {
        new ParamInfo(PACKAGE, "Package name", "ee.cyber.simplicitas.example",
                 "[a-z0-9]+(\\.[a-z0-9]+)*"),
         new ParamInfo(CLASS, "Class name prefix", "Example", "[A-Z]\\w*"),
         new ParamInfo(EXT, "DSL file extension", "bean", "\\w+"),
         new ParamInfo(ID, "Identifier of the DSL", "example_bean", "[\\w.]+"),
         new ParamInfo(DESCRIPTION, "Textual DSL description", "Example bean",
                 ".*"),
         new ParamInfo(BASE, "Base language to use", "bean", "(bean)|(empty)")
    };
    private Object[][] pathRepl = {
        { "ee/cyber/simplicitas/example", "" },
        { "Example", "" },
        { "extension", "" }
    };

    public static class ParamInfo {
        public String id;
        public String description;
        public String defaultValue;
        public String regexp;

        public ParamInfo(String id, String description, String defaultValue,
                String regexp) {
            this.id = id;
            this.description = description;
            this.defaultValue = defaultValue;
            this.regexp = regexp;
        }
    }

    public static ParamInfo getParamInfo(String name) {
        System.out.println("getParamInfo(" + name + ")");
        name = name.intern();

        for (int i = 0; i < PARAM_INFO.length; ++i) {
            if (PARAM_INFO[i].id == name) {
                return PARAM_INFO[i];
            }
        }

        throw new IllegalArgumentException("Invalid parameter: " + name);
    }

    public Wizard() {
        for (int i = 0; i < pathRepl.length; ++i) {
            pathRepl[i][0] = Pattern.compile((String) pathRepl[i][0]);
        }
    }

    public List missingParam() {
        ArrayList result = new ArrayList();
        for (int i = 0; i < PARAM_INFO.length; ++i) {
            if (params.get(PARAM_INFO[i].id) == null) {
                result.add(PARAM_INFO[i].id);
            }
        }
        return result;
    }

    public boolean set(String key, String value) throws BadParam {
        boolean known = false;
        key = key.intern();
        for (int i = 0; i < PARAM_INFO.length; ++i) {
            if (key == PARAM_INFO[i].id) {
                if (!value.matches(PARAM_INFO[i].regexp)) {
                    throw new BadParam(PARAM_INFO[i], value);
                }
                known = true;
            }
        }
        if (value == null)
            value = "";
        try {
            params.put(key, value.getBytes("UTF-8"));
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
        if (key == PACKAGE) {
            set("package-path", value.replace('.', File.separatorChar));
        } else if (key == "package-path") {
            pathRepl[0][1] = value;
        } else if (key == CLASS) {
            pathRepl[1][1] = value;
        } else if (key == EXT) {
            pathRepl[2][1] = value;
        } else if (key == ID) {
            if (!params.containsKey("lang")) {
                set("lang", value);
            }
            if (!params.containsKey("name")) {
                set("name", value.replace('_', '-'));
            }
        }
        return known;
    }

    private void store(String name, InputStream is) throws Exception {
        for (int i = 0; i < pathRepl.length; ++i) {
            name = ((Pattern) pathRepl[i][0]).matcher(name)
                                    .replaceFirst((String) pathRepl[i][1]);
        }
        name.replace('/', File.separatorChar);
        File f = new File(target, name);
        // Check whether hidden (eclipse) files are allowed.
        if (!copyDotFiles && f.getName().startsWith(".")) {
            return;
        }
        f.getParentFile().mkdirs();
        OutputStream os = new BufferedOutputStream(new FileOutputStream(f));
        try {
            byte[] buf = this.buf;
            int rc = 0;
            do {
                int n = 0, pos = 0;
                while (n < 30000 && (rc = is.read(buf, n, buf.length - n)) > 0) {
                    n += rc;
                }
                for (int i = 1; i < n; ++i) {
                    if (buf[i] != '{' || buf[i - 1] != '#') {
                        continue;
                    }
                    int start = i + 1;
                    while (++i < n && buf[i] != '}') {
                    }
                    if (i == start || i - start > 32)
                        continue;
                    // latin1 to avoid decode errors
                    String s = new String(buf, start, i - start, "ISO-8859-1");
                    byte[] repl = (byte[]) params.get(s);
                    if (repl == null)
                        continue;
                    start -= 2;
                    if (start > pos) {
                        os.write(buf, pos, start - pos);
                    }
                    pos = i + 1;
                    os.write(repl);
                }
                if (pos < n)
                    os.write(buf, pos, n - pos);
            } while (rc > 0);
        } finally {
            os.close();
        }
    }

    public void unzip() throws Exception {
        String src = "src-"
            + new String((byte[]) params.get(BASE), "UTF-8")
            + ".zip";
        InputStream is = getClass().getClassLoader().getResourceAsStream(src);
        if (is == null)
            throw new Exception(src + " lost");
        ZipInputStream zip = new ZipInputStream(is);
        try {
            for (ZipEntry entry; (entry = zip.getNextEntry()) != null;) {
                if (!entry.isDirectory())
                    store(entry.getName(), zip);
            }
        } finally {
            zip.close();
        }
    }

    public static void main(String[] argv) throws Exception {
        Wizard wiz = new Wizard();
        int i = 0;
        try {
            while (i + 1 < argv.length) {
                String opt = argv[i];
                String value = argv[i + 1];
                if (!opt.startsWith("-"))
                    break;
                if ("-d".equals(opt)) {
                    wiz.target = value;
                } else if (!wiz.set(opt.substring(1), value)) {
                    break;
                }
                i += 2;
            }
        } catch (BadParam ex) {
            System.err.println(ex.getMessage());
            System.exit(1);
        }
        List missing = wiz.missingParam();
        if (i < argv.length) {
            System.err.println("Unknown option: " + argv[i]);
        } else if (wiz.target == null) {
            System.err.println("Missing option: -d target");
        } else if (missing.size() != 0) {
            System.err.println("Missing parameters: " + missing);
        } else {
            wiz.unzip();
            System.out.println("Created example into " + wiz.target);
            return;
        }
        System.err.println("\n   -d <Target directory>");
        for (i = 0; i < PARAM_INFO.length; ++i) {
            System.err.println("   -" + PARAM_INFO[i].id +
                " <" + PARAM_INFO[i].description + ">\n\t/"
                + PARAM_INFO[i].regexp + "/ like \""
                + PARAM_INFO[i].defaultValue + "\"");
        }
        System.exit(1);
    }
}
