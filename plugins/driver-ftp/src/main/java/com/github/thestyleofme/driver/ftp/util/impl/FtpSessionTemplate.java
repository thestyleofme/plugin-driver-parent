package com.github.thestyleofme.driver.ftp.util.impl;

import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.ftp.util.SessionTemplate;
import com.github.thestyleofme.plugin.framework.constants.BaseConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPReply;

/**
 * FTP 工具类
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 16:12
 */
@Slf4j
public class FtpSessionTemplate implements SessionTemplate {

    private FTPClient ftpClient = null;

    public FtpSessionTemplate(String host, int port, String userName, String password) throws Exception {
        ftpClient = new FTPClient();
        ftpClient.setControlEncoding(StandardCharsets.UTF_8.name());
        //连接ftp服务器
        ftpClient.connect(host, port);
        //登录ftp服务器
        ftpClient.login(userName, password);
        //是否成功登录服务器
        int replyCode = ftpClient.getReplyCode();
        if (!FTPReply.isPositiveCompletion(replyCode)) {
            throw new DriverException("ftp服务器连接失败");
        }
        ftpClient.enterLocalPassiveMode();
    }

    /**
     * 上传文件
     *
     * @param remoteFile 远程文件
     * @param in         输入流
     * @throws Exception 异常
     */
    @Override
    public void uploadFile(String remoteFile, InputStream in) throws Exception {
        ftpClient.setFileType(FTPClient.BINARY_FILE_TYPE);
        ftpClient.setControlEncoding("UTF-8");

        int index = remoteFile.lastIndexOf(BaseConstants.Symbol.SLASH);
        String tmpPath = remoteFile.substring(0, index);
        if (tmpPath.startsWith(BaseConstants.Symbol.SLASH)) {
            tmpPath = tmpPath.substring(1);
        }
        String fileName = remoteFile.substring(index + 1);
        if (!ftpClient.makeDirectory(tmpPath)) {
            log.warn("目录创建失败或者已经存在！");
        }
        if (ftpClient.changeWorkingDirectory(StringUtils.isBlank(tmpPath) ? BaseConstants.Symbol.POINT : tmpPath)) {
            log.info("切换工作目录！");
            if (ftpClient.storeFile(fileName, in)) {
                log.info("上传成功！");
            } else {
                log.error("上传失败");
            }
        }

    }

    /**
     * 下载文件
     *
     * @param remotePathFile 远程文件路径
     * @param localPathFile  当前文件路径
     * @throws Exception 异常
     */
    @Override
    public void downloadFile(String remotePathFile, String localPathFile) throws Exception {
        ftpClient.setFileType(FTPClient.BINARY_FILE_TYPE);
        int index = remotePathFile.lastIndexOf(BaseConstants.Symbol.SLASH);
        String tmpPath = remotePathFile.substring(0, index);
        if (tmpPath.startsWith(BaseConstants.Symbol.SLASH)) {
            tmpPath = tmpPath.substring(1);
        }
        String fileName = remotePathFile.substring(index + 1);
        if (ftpClient.changeWorkingDirectory(StringUtils.isBlank(tmpPath) ? BaseConstants.Symbol.POINT : tmpPath)) {
            log.info("切换工作目录！");
            try (OutputStream os = new FileOutputStream(localPathFile)) {
                if (ftpClient.retrieveFile(fileName, os)) {
                    log.info("下载成功");
                } else {
                    log.error("下载失败");
                }
            }
        }
    }

    /**
     * 下载文件
     *
     * @param remotePathFile 远程文件路径
     * @throws Exception 异常
     */
    @Override
    public InputStream getFileInputStream(String remotePathFile) throws Exception {
        ftpClient.setFileType(FTPClient.BINARY_FILE_TYPE);
        int index = remotePathFile.lastIndexOf(BaseConstants.Symbol.SLASH);
        String tmpPath = remotePathFile.substring(0, index);
        if (tmpPath.startsWith(BaseConstants.Symbol.SLASH)) {
            tmpPath = tmpPath.substring(1);
        }
        String fileName = remotePathFile.substring(index + 1);
        if (ftpClient.changeWorkingDirectory(StringUtils.isBlank(tmpPath) ? BaseConstants.Symbol.POINT : tmpPath)) {
            log.info("切换工作目录！");
            return ftpClient.retrieveFileStream(fileName);
        } else {
            throw new DriverException("下载失败");
        }
    }

    public FTPFile[] getDirList(String path) throws Exception {
        if (path.startsWith(BaseConstants.Symbol.SLASH)) {
            path = path.substring(1);
        }
        if (ftpClient.changeWorkingDirectory(StringUtils.isBlank(path) ? BaseConstants.Symbol.POINT : path)) {
            return ftpClient.listFiles();
        }
        return new FTPFile[0];
    }

    @Override
    public Map<String, Boolean> fileMap(String filePath) throws Exception {
        FTPFile[] dirList = getDirList(filePath);
        Map<String, Boolean> map = new HashMap<>(dirList.length);
        for (FTPFile ftpFile : dirList) {
            map.put(ftpFile.getName(), ftpFile.isDirectory());
        }
        return map;
    }

    @Override
    public void close() throws Exception {
        if (ftpClient != null) {
            ftpClient.logout();
            ftpClient.disconnect();
        }
    }

}
