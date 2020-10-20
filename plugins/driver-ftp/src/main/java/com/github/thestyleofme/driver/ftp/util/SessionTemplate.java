package com.github.thestyleofme.driver.ftp.util;

import java.io.InputStream;
import java.util.Map;
import java.util.Properties;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.core.infra.utils.Conf;
import com.github.thestyleofme.driver.ftp.constant.Key;
import com.github.thestyleofme.driver.ftp.constant.Protocol;
import com.github.thestyleofme.driver.ftp.util.impl.FtpSessionTemplate;
import com.github.thestyleofme.driver.ftp.util.impl.SftpSessionTemplate;

/**
 * SessionTemplate 接口
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 16:58
 */
public interface SessionTemplate extends AutoCloseable {

    /**
     * 获取实例
     *
     * @param properties 参数
     * @return SessionTemplate
     */
    static SessionTemplate getInstance(Properties properties) {
        String protocol = Conf.require(properties, Key.PROTOCOL);
        String host = Conf.require(properties, Key.HOST);
        String port = Conf.require(properties, Key.PORT);
        String username = Conf.require(properties, Key.USERNAME);
        String password = Conf.require(properties, Key.PASSWORD);
        SessionTemplate sessionTemplate;
        try {
            switch (Protocol.valueOf(protocol.toUpperCase())) {
                case FTP:
                    sessionTemplate = new FtpSessionTemplate(host, Integer.parseInt(port), username, password);
                    break;
                case SFTP:
                    sessionTemplate = new SftpSessionTemplate(host, Integer.parseInt(port), username, password);
                    break;
                default:
                    throw new IllegalArgumentException("Error protocol:" + protocol);
            }
        } catch (Exception e) {
            throw new DriverException("Connect Server error", e);
        }
        return sessionTemplate;
    }

    /**
     * 上传文件
     *
     * @param remoteFile 远程文件
     * @param in         输入流
     * @throws Exception 异常
     */
    void uploadFile(String remoteFile, InputStream in) throws Exception;

    /**
     * 下载文件
     *
     * @param remotePathFile 远程文件路径
     * @param localPathFile  当前文件路径
     * @throws Exception 异常
     */
    void downloadFile(String remotePathFile, String localPathFile) throws Exception;

    /**
     * @param pathFile 远程文件
     * @return InputStream InputStream
     * @throws Exception Exception
     */
    InputStream getFileInputStream(String pathFile) throws Exception;

    /**
     * 获取文件map,key文件名，value是否为目录
     *
     * @param filePath
     * @return
     */
    Map<String, Boolean> fileMap(String filePath) throws Exception;
}
