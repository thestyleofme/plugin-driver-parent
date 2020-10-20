package com.github.thestyleofme.driver.core.infra.utils;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import freemarker.core.TemplateClassResolver;
import freemarker.core.TemplateConfiguration;
import freemarker.core.UndefinedOutputFormat;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

/**
 * <p>
 *
 * </p>
 *
 * @author JupiterMouse 2020/08/17
 * @since 1.0
 */
public class LocalFreeMakerUtil {

    private LocalFreeMakerUtil() {

    }

    /**
     * 解析文本
     *
     * @param text     文本
     * @param paramMap 参数Map
     * @return 解析后的文本
     * @throws IOException       IO异常
     * @throws TemplateException 模版异常
     */
    public static String parserText(String text, Map<String, Object> paramMap) throws IOException, TemplateException {
        Configuration freeMarkerConfig = LocalFreeMakerUtil.getConfiguration();
        TemplateConfiguration tCfg = new TemplateConfiguration();
        tCfg.setParentConfiguration(freeMarkerConfig);
        Template template = new Template(null, null, new StringReader(text), freeMarkerConfig, tCfg, null);
        tCfg.apply(template);
        StringWriter writer = new StringWriter();
        template.process(paramMap, writer);
        return writer.toString();
    }

    /**
     * freemarker 默认配置
     *
     * @return Configuration
     */
    private static Configuration getConfiguration() {
        // 创建配置类，配置类应尽可能重用，不必再创建以提高性能
        Configuration freeMarkerConfig = new Configuration(Configuration.getVersion());
        freeMarkerConfig.setNewBuiltinClassResolver(TemplateClassResolver.ALLOWS_NOTHING_RESOLVER);
        // 设置模板异常处理器
        freeMarkerConfig.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
        // FreeMarker 会在模板执行期间使用 freemarker.runtime 记录异常，即便异常继续增加，最终由 Template.process 或 Environment.process 抛出。
        // (那些都是从应用程序或框架中调用模板时的API调用。) 良好的应用程序会记录它们抛出的异常，极少数情况下是处理它们而不去记录日志。
        // 但是FreeMarker已经记录了异常，那么就会得到比期望的多一条日志记录。setLogTemplateExceptions(false) 可修复
        freeMarkerConfig.setLogTemplateExceptions(false);
        // 设置#attempt块异常处理器
        freeMarkerConfig.setAttemptExceptionReporter((te, env) -> {
            // Suppress it
        });

        // 时区设置
        freeMarkerConfig.setLocale(Locale.CHINESE);
        freeMarkerConfig.setTimeZone(TimeZone.getTimeZone("Asia/Shanghai"));
        // 	设置默认输出格式。 可通过OutputFormat寻找需要的类型
        freeMarkerConfig.setOutputFormat(UndefinedOutputFormat.INSTANCE);
        // 设置字符集
        freeMarkerConfig.setOutputEncoding("UTF-8");
        //  设置浮点类型
        freeMarkerConfig.setNumberFormat("#");
        return freeMarkerConfig;
    }
}
