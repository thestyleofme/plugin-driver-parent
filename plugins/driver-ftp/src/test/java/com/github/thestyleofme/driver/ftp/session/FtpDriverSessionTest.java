package com.github.thestyleofme.driver.ftp.session;

import java.util.List;
import java.util.Map;
import java.util.Properties;

import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.ftp.util.SessionTemplate;
import org.junit.Before;
import org.junit.Test;

public class FtpDriverSessionTest {

    private FtpDriverSession ftpDriverSession;

    @Before
    public void setup() {
        Properties properties = new Properties();
        properties.setProperty("protocol", "sftp");
        properties.setProperty("host", "172.23.16.63");
        properties.setProperty("port", "22");
        properties.setProperty("username", "root");
        properties.setProperty("password", "m8rW2EQ0iDCcWlbH");
        ftpDriverSession = new FtpDriverSession(SessionTemplate.getInstance(properties));
    }

    @Test
    public void getCsvColumn() {
        ftpDriverSession.getCsvColumns("/data/hdsp/infra/ftp-file/test_partitio*", ",", true)
                .forEach(System.out::println);
    }

    @Test
    public void testGetData() {
        ResponseData<?> responseData = ftpDriverSession.get(
                Payload.of()
                        .putArgs("filePath", "/data/hdsp/infra/ftp-file/test_partitio1.csv")
                        .putArgs("delimiter", ",")
                        .putArgs("skipHeader", false));
        System.out.println(responseData.getData());
    }

    @Test
    public void testSql() {
        String sql = "{\n" +
                "\"filePath\": \"/data/hdsp/infra/ftp-file/test_partitio1.csv\",\n" +
                "\"delimiter\": \",\",\n" +
                "\"skipHeader\": false\n" +
                "}";
        List<Map<String, Object>> maps = ftpDriverSession.executeOneQuery(null, sql);
        maps.forEach(System.out::println);
    }
}
