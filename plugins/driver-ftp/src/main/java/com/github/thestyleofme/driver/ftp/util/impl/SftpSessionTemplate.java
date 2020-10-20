package com.github.thestyleofme.driver.ftp.util.impl;

import java.io.*;
import java.util.*;

import com.github.thestyleofme.driver.core.infra.exceptions.DriverException;
import com.github.thestyleofme.driver.ftp.util.SessionTemplate;
import com.jcraft.jsch.*;
import lombok.extern.slf4j.Slf4j;

/**
 * Sftp
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 16:11
 */
@Slf4j
public class SftpSessionTemplate implements SessionTemplate {

    private static final String SPLIT_PATTERN = "/";

    private static final int TIMEOUT = 60000;

    private Session session = null;
    private ChannelSftp channel = null;


    public SftpSessionTemplate(String host,
                               int port,
                               String userName,
                               String password) throws Exception {
        JSch jsch = new JSch();
        // 根据用户名，主机ip，端口获取一个Session对象
        session = jsch.getSession(userName, host, port);
        // 设置密码
        session.setPassword(password);
        // 为Session对象设置properties
        Properties config = new Properties();
        config.put("StrictHostKeyChecking", "no");
        session.setConfig(config);
        // 通过Session建立链接
        session.connect();
        // 打开SFTP通道
        channel = (ChannelSftp) session.openChannel("sftp");
        // 建立SFTP通道的连接
        channel.connect();
    }

    /**
     * 获取session
     *
     * @param host     服务IP
     * @param port     端口
     * @param username 用户名
     * @param password 密码
     * @throws JSchException JSchException
     * @author abigballofmud 2020-2-5 16:20:45
     */
    private void getSession(String host, int port, String username, String password) throws JSchException {
        // 创建JSch对象
        JSch jsch = new JSch();
        // 根据用户名，主机ip，端口获取一个Session对象
        session = jsch.getSession(username, host, port);
        log.info("Session created...");
        if (!Objects.isNull(password)) {
            // 设置密码
            session.setPassword(password);
        }
        // 为Session对象设置properties
        Properties config = new Properties();
        config.put("StrictHostKeyChecking", "no");
        session.setConfig(config);
        // 设置timeout时间
        session.setTimeout(TIMEOUT);
        // 通过Session建立连接
        session.connect();
        log.info("Session connected, Opening Channel...");
    }

    /**
     * 自动关闭资源
     */
    @Override
    public void close() {
        if (channel != null) {
            channel.disconnect();
        }
        if (session != null) {
            session.disconnect();
        }
    }

    /**
     * @param path path
     * @return List<ChannelSftp.LsEntry>
     * @throws SftpException s
     * @author zhilong.deng
     * @date 2018/7/16 21:57
     */
    public List<ChannelSftp.LsEntry> getDirList(String path) throws SftpException {
        List<ChannelSftp.LsEntry> list = new ArrayList<>();
        if (channel != null) {
            // http://epaul.github.io/jsch-documentation/javadoc/
            // The pattern can contain glob pattern wildcards (* or ?) in the last component (i.e. after the last /)
            Vector vv = channel.ls(path);
            if (vv == null || vv.size() == 0) {
                return list;
            } else {
                Object[] aa = vv.toArray();
                for (Object obj : aa) {
                    ChannelSftp.LsEntry temp = (ChannelSftp.LsEntry) obj;
                    list.add(temp);
                }
            }
        }
        return list;
    }

    @Override
    public Map<String, Boolean> fileMap(String filePath) throws Exception {
        List<ChannelSftp.LsEntry> dirList = getDirList(filePath);
        Map<String, Boolean> map = new HashMap<>(dirList.size());
        for (ChannelSftp.LsEntry lsEntry : dirList) {
            map.put(lsEntry.getFilename(), lsEntry.getAttrs().isDir());
        }
        return map;
    }

    /**
     * @param path   path
     * @param suffix suffix
     * @return List<ChannelSftp.LsEntry>
     * @throws SftpException s
     * @author zhilong.deng
     * @date 2018/7/16 21:57
     */
    public List<ChannelSftp.LsEntry> getFiles(String path, String suffix) throws SftpException {
        List<ChannelSftp.LsEntry> list = new ArrayList<>();
        if (channel != null) {
            channel.ls(path, lsEntry -> {
                if (!lsEntry.getAttrs().isDir() && lsEntry.getFilename().endsWith(suffix)) {
                    list.add(lsEntry);
                }
                return 0;
            });

        }
        return list;
    }

    /**
     * 下载文件
     *
     * @param remotePathFile 远程文件
     * @param localPathFile  本地文件[绝对路径]
     * @throws SftpException SftpException
     * @throws IOException   IOException
     */
    @Override
    public void downloadFile(String remotePathFile, String localPathFile) throws Exception {
        try (FileOutputStream os = new FileOutputStream(new File(localPathFile))) {
            if (channel == null) {
                throw new IOException("sftp server not login");
            }
            channel.get(remotePathFile, os);
        }
    }

    /**
     * 服务器文件下载到本地
     *
     * @param remotePath 服务器文件路径
     * @param os         本地输入流
     */
    public void downloadFileToLocal(String remotePath, OutputStream os) throws SftpException {
        channel.get(remotePath, os);
    }

    /**
     * @param pathFile 远程文件
     * @return InputStream InputStream
     * @throws Exception Exception
     */
    @Override
    public InputStream getFileInputStream(String pathFile) throws Exception {
        return channel.get(pathFile);
    }

    public OutputStream put(String des) throws SftpException {
        return channel.put(des);
    }

    /**
     * 上传文件
     *
     * @param remoteFile 远程文件
     * @param localFile  l
     * @throws SftpException s
     * @throws IOException   i
     */
    public void uploadFileWithStr(String remoteFile, String localFile) throws SftpException, IOException {
        try (FileInputStream in = new FileInputStream(new File(localFile))) {
            if (channel == null) {
                throw new IOException("sftp server not login");
            }
            channel.put(in, remoteFile);
        }
    }

    /**
     * 上传文件
     *
     * @param remoteFile 远程文件
     * @param in         InputStream
     * @throws Exception s
     */
    @Override
    public void uploadFile(String remoteFile, InputStream in) throws Exception {
        if (channel == null) {
            throw new DriverException("sftp server not login");
        }
        channel.put(in, remoteFile);
    }

    /**
     * 获取文件大小
     *
     * @param filePath filePath
     * @return Long
     * @throws SftpException SftpException
     * @throws IOException   IOException
     */
    public Long getFileSize(String filePath) throws SftpException, IOException {
        if (channel == null) {
            throw new IOException("sftp server not login");
        }
        return channel.lstat(filePath).getSize();
    }

    /**
     * 移动文件
     *
     * @param sourcePath sourcePath
     * @param targetPath targetPath
     * @throws SftpException SftpException
     * @throws IOException   IOException
     */
    public void rename(String sourcePath, String targetPath) throws SftpException, IOException {
        if (channel == null) {
            throw new IOException("sftp server not login");
        }
        channel.rename(sourcePath, targetPath);
    }

    /**
     * 删除服务器文件
     *
     * @param filePath filePath
     * @throws SftpException SftpException
     * @throws IOException   IOException
     */
    public void remove(String filePath) throws SftpException, IOException {
        if (channel == null) {
            throw new IOException("sftp server not login");
        }

        try {
            channel.rename(filePath, filePath + ".bak");
            channel.rm(filePath);
        } catch (SftpException e) {
            log.error("文件不存在");
        }
    }

    public boolean exist(String filePath) throws IOException {
        if (channel == null) {
            throw new IOException("sftp server not login");
        }

        SftpATTRS stat = null;
        try {
            stat = channel.stat(filePath);
        } catch (SftpException e) {
            // ignore
        }
        return stat != null;
    }

    /**
     * 创建目录
     *
     * @param path 目录
     * @param dir  目录
     * @throws SftpException s
     * @throws IOException   i
     */
    public synchronized void mkdir(String path, String dir) throws SftpException, IOException {
        StringBuilder builder = new StringBuilder();
        builder.append(path);
        if (channel == null) {
            throw new IOException("sftp server not login");
        }
        for (String d : dir.split(SPLIT_PATTERN)) {
            builder.append(SPLIT_PATTERN).append(d);
            try {
                channel.ls(builder.toString());
            } catch (SftpException e) {
                channel.mkdir(builder.toString());
            }
        }
    }


    /**
     * @param remoteFile 备份文件的路径
     * @param backupName 备份路径
     * @throws SftpException s
     * @throws IOException   i
     * @author yuan.meng
     * @date 2019-10-09 17:46:14
     */
    public void backup(String remoteFile, String backupName) throws SftpException, IOException {
        if (channel == null) {
            throw new IOException("sftp server not login");
        }
        try {
            channel.rename(remoteFile, backupName);
        } catch (SftpException e) {
            log.info("备份出现问题");
        }
    }
}