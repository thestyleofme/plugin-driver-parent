package com.github.codingdebugallday.driver.common.infra.autoconfigure;

import com.github.codingdebugallday.driver.common.app.service.PluginMinioService;
import com.github.codingdebugallday.driver.common.app.service.impl.PluginMinioServiceImpl;
import io.minio.MinioClient;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * <p>
 * 只在生产环境下有效，初始化minio
 * </p>
 *
 * @author isaac 2020/7/14 13:49
 * @since 1.0.0
 */
@Configuration
@EnableConfigurationProperties(MinioProperties.class)
@ConditionalOnExpression("'${plugin.runMode}'.equalsIgnoreCase('prod') || '${plugin.runMode}'.equalsIgnoreCase('deployment')")
public class MinioConfiguration {

    private final MinioProperties minioProperties;

    public MinioConfiguration(MinioProperties minioProperties) {
        this.minioProperties = minioProperties;
    }

    @Bean
    @ConditionalOnProperty(prefix = "plugin", name = "store-type", havingValue = "minio")
    public MinioClient pluginMinioClient() {
        return MinioClient.builder()
                .endpoint(minioProperties.getEndpoint())
                .credentials(minioProperties.getAccesskey(), minioProperties.getSecretKey())
                .build();
    }

    @Bean
    @ConditionalOnProperty(prefix = "plugin", name = "store-type", havingValue = "minio")
    public PluginMinioService pluginMinioService(MinioClient pluginMinioClient) {
        return new PluginMinioServiceImpl(pluginMinioClient);
    }

}
