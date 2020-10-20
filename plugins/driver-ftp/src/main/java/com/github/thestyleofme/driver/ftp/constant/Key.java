package com.github.thestyleofme.driver.ftp.constant;

/**
 * 关键字
 *
 * @author terry
 * @version 1.0
 * @date 2020/8/18 16:35
 */
public interface Key {


    Character DEFAULT_DELIMITER = ',';
    Boolean DEFAULT_SKIP_HEADER = false;

    /**
     * 协议
     */
    String PROTOCOL = "protocol";
    /**
     * 主机
     */
    String HOST = "host";
    /**
     * 端口
     */
    String PORT = "port";
    /**
     * 用户
     */
    String USERNAME = "username";
    /**
     * 密码
     */
    String PASSWORD = "password";

    /**
     * 文件路径
     */
    String FILE_PATH = "filePath";

    /**
     * 分隔符
     */
    String DELIMITER = "delimiter";

    /**
     * 是否跳过表头
     */
    String SKIP_HEADER = "skipHeader";
}
