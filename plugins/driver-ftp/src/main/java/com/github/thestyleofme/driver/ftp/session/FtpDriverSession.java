package com.github.thestyleofme.driver.ftp.session;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SessionTool;
import com.github.thestyleofme.driver.core.app.service.session.SqlResponse;
import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.meta.Column;
import com.github.thestyleofme.driver.ftp.constant.Key;
import com.github.thestyleofme.driver.ftp.util.CsvUtil;
import com.github.thestyleofme.driver.ftp.util.SessionTemplate;
import com.github.thestyleofme.plugin.core.infra.constants.BaseConstant;
import com.github.thestyleofme.plugin.core.infra.utils.JsonUtil;
import com.github.thestyleofme.plugin.framework.constants.BaseConstants;
import com.jcraft.jsch.SftpException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.util.CollectionUtils;

/**
 * FTP 驱动Session
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 17:39
 */
@Slf4j
public class FtpDriverSession implements DriverSession, SessionTool {

    private final SessionTemplate sessionTemplate;

    public FtpDriverSession(SessionTemplate sessionTemplate) {
        this.sessionTemplate = sessionTemplate;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public List<Column> columnMetaData(String schema, String tableName) {
        return this.getCsvColumns(tableName, BaseConstant.Symbol.COMMA, Key.DEFAULT_SKIP_HEADER);
    }

    @Override
    public List<Column> getCsvColumns(String filePath, String delimiter, Boolean skipHeader) {
        boolean includeHeader = Optional.ofNullable(skipHeader).orElse(Key.DEFAULT_SKIP_HEADER);
        String delim = unicode2String(delimiter);

        List<Column> columns;
        // 获取第一个文件
        String firstFilePath = getFirstFilePath(filePath);
        try (
                InputStream inputStream = sessionTemplate.getFileInputStream(firstFilePath);
                BufferedReader br = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
        ) {
            // 只读第一行
            String line = br.readLine();
            if (line.length() != 0) {
                String[] lineArray = CsvUtil.splitOneLine(line, delim.charAt(0));
                columns = new ArrayList<>(lineArray.length);
                for (int i = 0; i < lineArray.length; i++) {
                    if (includeHeader) {
                        columns.add(Column.builder().columnIndex(i).columnName(lineArray[i]).typeName("String").build());
                    } else {
                        columns.add(Column.builder().columnIndex(i).columnName(String.valueOf(i)).typeName("String").build());
                    }
                }
            } else {
                throw new DriverException(String.format("empty file content, %s", filePath));
            }
        } catch (Exception e) {
            throw new DriverException("error.datasource.ftp.columns", e);
        }
        return columns;
    }

    /**
     * 例子：
     * {
     * "filePath": "/opt/hdsp/infra/aaa.csv",
     * "delimiter": ",",
     * "skipHeader": true
     * }
     * 或
     * {
     * "filePath": "/opt/hdsp/infra/aaa.csv",
     * "delimiter": "\\u002C",
     * "skipHeader": true
     * }
     *
     * @param payload 参数信息
     * @return 数据
     */
    @Override
    public ResponseData<?> get(Payload payload) {
        String filePath = payload.getOrThrow(Key.FILE_PATH);
        String delimiter = unicode2String((String) payload.getOrDefault(Key.DELIMITER, Key.DEFAULT_DELIMITER));
        Boolean skipHeader = (Boolean) payload.getOrDefault(Key.SKIP_HEADER, Key.DEFAULT_SKIP_HEADER);
        List<Map<String, Object>> result = new ArrayList<>();
        List<String> headers = null;
        try (
                InputStream inputStream = sessionTemplate.getFileInputStream(filePath);
                BufferedReader br = new BufferedReader(new InputStreamReader(inputStream, Charset.defaultCharset()));
        ) {
            String line;
            while (StringUtils.isNotBlank(line = br.readLine())) {
                String[] lineArray = CsvUtil.splitOneLine(line, delimiter.charAt(0));
                if (CollectionUtils.isEmpty(headers)) {
                    if (skipHeader) {
                        headers = Arrays.asList(lineArray);
                        continue;
                    } else {
                        headers = new ArrayList<>(lineArray.length);
                        for (int i = 0; i < lineArray.length; i++) {
                            headers.add(String.valueOf(i));
                        }
                    }
                }
                Map<String, Object> lineRecord = new HashMap<>(lineArray.length);
                for (int i = 0; i < lineArray.length; i++) {
                    lineRecord.put(headers.get(i), lineArray[i]);
                }
                result.add(lineRecord);
            }
        } catch (Exception e) {
            throw new DriverException("error.datasource.ftp.columns", e);
        }
        return ResponseData.builder().data(result).build();
    }

    @Override
    public List<String> schemaList(String... params) {
        return Collections.singletonList("default");
    }

    @Override
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> executeOneQuery(String schema, String sql) {
        Payload payload = JsonUtil.toObj(sql, Payload.class);
        return (List<Map<String, Object>>) get(payload).getData();
    }


    @Override
    public Map<String, SqlResponse> executeAllDetail(String schema, String text, boolean transactionFlag, boolean savepointFlag, boolean resultFlag) {
        return Collections.singletonMap("RESULT_1",
                SqlResponse.builder().isSuccess(true).sql(text).data(this.executeOneQuery(schema, text)).build());
    }

    @Override
    public List<List<Map<String, Object>>> executeAll(String schema, String text, boolean transactionFlag, boolean savepointFlag, boolean resultFlag) {
        return Collections.singletonList(this.executeOneQuery(schema, text));
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable) {
        List<Map<String, Object>> list = this.executeOneQuery(schema, text);
        return Collections.singletonList(new PageImpl<>(list, pageable, list.size()));
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text) {
        return this.executePageAll(schema, text, Pageable.unpaged());
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable, boolean transactionFlag, boolean resultFlag) {
        return this.executePageAll(schema, text, pageable);
    }

    /**
     * unicode字符串转字符串
     *
     * @param unicode unicode
     * @return String
     */
    private String unicode2String(String unicode) {
        String result = unicode;
        if (unicode.startsWith("\\u")) {
            StringBuilder sb = new StringBuilder();
            String[] hex = unicode.split("\\\\u");

            for (int i = 1; i < hex.length; i++) {
                // 转换出每一个代码点
                int data = Integer.parseInt(hex[i], 16);
                // 追加成string
                sb.append((char) data);
            }
            result = sb.toString();
        }
        log.info("============= delimiter:" + unicode);
        if (result.length() > 1) {
            throw new DriverException("error delimiter");
        }
        return result;
    }

    /**
     * 获取第一个文件
     *
     * @param filePath 文件路径
     * @return 第一个文件路径
     * @throws SftpException SFTP异常
     */
    private String getFirstFilePath(String filePath) {
        if (StringUtils.contains(filePath, BaseConstants.Symbol.COMMA)) {
            filePath = filePath.split(BaseConstants.Symbol.COMMA)[0];
        }
        //*和？的限制
        if (filePath.contains("*") || filePath.contains("?")) {
            String parentPath = getRegexPathParentPath(filePath);
            // 获取目录下所有的文件和子目录
            Map<String, Boolean> fileMap = null;
            try {
                fileMap = sessionTemplate.fileMap(filePath);
            } catch (Exception e) {
                throw new DriverException("get first file error", e);
            }
            for (String fileName : fileMap.keySet()) {
                if (!StringUtils.startsWith(fileName, ".")
                        && !fileMap.get(fileName)) {
                    // 目录与 隐藏文件夹跳过
                    return parentPath + BaseConstants.Symbol.SLASH + fileName;
                }
            }
            throw new DriverException("No file available");
        }
        return filePath;
    }

    /**
     * 获取父目录
     *
     * @param regexPath 正则路径
     * @return 父目录路径
     */
    private String getRegexPathParentPath(String regexPath) {
        int lastDirSeparator = regexPath.lastIndexOf(BaseConstants.Symbol.SLASH);
        String parentPath = regexPath.substring(0, lastDirSeparator + 1);
        if (parentPath.contains("*") || parentPath.contains("?")) {
            throw new DriverException(
                    String.format("配置项目path中：[%s]不合法，目前只支持在最后一级目录使用通配符*或者?", regexPath));
        }
        return parentPath;
    }
}
