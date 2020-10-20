package com.github.thestyleofme.driver.ftp.util;

import java.io.IOException;
import java.io.StringReader;

import com.csvreader.CsvReader;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

/**
 * Csv 工具类
 *
 * @author terry
 * @version 1.0
 * @date 2020/2/7 16:12
 */
@UtilityClass
@Slf4j
public class CsvUtil {

    public static final char DEFAULT_DELIMITER = ',';

    /**
     * CSV分割解析
     *
     * @param inputLine 输入待分隔字符串
     * @param delimiter 字符串分割符
     * @return 分隔符分隔后的字符串数组，出现异常时返回为null 支持转义，即数据中可包含分隔符
     */
    public static String[] splitOneLine(String inputLine, char delimiter) {
        String[] splitedResult = null;
        if (null != inputLine) {
            try {
                CsvReader csvReader = new CsvReader(new StringReader(inputLine));
                csvReader.setDelimiter(delimiter);

                if (csvReader.readRecord()) {
                    splitedResult = csvReader.getValues();
                }
            } catch (IOException e) {
                log.error("split Line error", e);
            }
        }
        return splitedResult;
    }
}
