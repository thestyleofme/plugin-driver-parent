package com.github.thestyleofme.plugin.core.infra.autoconfigure;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/14 13:40
 * @since 1.0.0
 */
@ConfigurationProperties(prefix = "plugin.minio")
@Data
public class MinioProperties {

    /**
     * 连接url
     */
    private String endpoint;
    /**
     * 用户名
     */
    private String accesskey;
    /**
     * 密码
     */
    private String secretKey;

}
